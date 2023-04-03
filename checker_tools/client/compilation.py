import subprocess
from pathlib import Path
import re
import os
import json
from typing import List, Optional, Tuple

FA2_VIEWS_PAT = (
    r"let view_(\S+) *\([^:]*: *(.*) \* (mock_fa2_state)\) *: *([^=]*)"
)
WTEZ_VIEWS_PAT = (
    r"let view_(\S+) *\([^:]*: *(.*) \* (wtez_state)\) *: *([^=]*)"
)
WCTEZ_VIEWS_PAT = (
    r"let view_(\S+) *\([^:]*: *(.*) \* (wctez_state)\) *: *([^=]*)"
)
CHECKER_VIEWS_PAT = (
    r"let wrapper_view_(\S+) *\([^:]*: *(.*) \* (wrapper)\) *: *([^=]*)"
)
CHECKER_ENTRYPOINTS_PAT = r"let lazy_id_(\S+) *= \(*(\d*)\)"


def find_views(
    src_file: Path, view_pattern: str
) -> List[Tuple[str, str, str]]:
    with open(src_file) as f:
        code = f.read()
        raw_views = re.findall(view_pattern, code)
        return [  # We keep the name of the state type for later
            (name, arg_type.strip(), ret_type.strip(), state_type.strip())
            for (name, arg_type, state_type, ret_type) in raw_views
        ]


def find_entrypoints(
    src_file: Path, view_pattern: str
) -> List[Tuple[str, int]]:
    with open(src_file) as f:
        code = f.read()
        raw_views = re.findall(view_pattern, code)
        return [(int(id), name) for (name, id) in raw_views]


def ligo_compile_json(src_file: Path, expr: str):
    try:
        res = subprocess.run(
            [
                "ligo",
                "compile",
                "expression",
                "cameligo",
                "--init-file",
                src_file,
                "--michelson-format",
                "json",
                expr,
            ],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError as e:
        print(e.stdout)
        print(e.stderr)
        raise e
    return res.stdout


def ligo_compile_type(*, src_file: Path, typ: str):
    expr = f"fun (i: bytes) -> (Bytes.unpack i: ({typ}) option)"
    compiled = ligo_compile_json(src_file, expr)
    compiled = json.loads(compiled)
    # returns [{'prim': 'UNPACK', 'args': [actual type]}]
    return compiled[0]["args"][0]


def ligo_compile(*, src_file: Path, entrypoint: str, out_file: Path):
    """Compiles an mligo file into michelson using ligo"""
    try:
        res = subprocess.run(
            [
                "ligo",
                "compile",
                "contract",
                str(src_file),
                "--entry-point",
                entrypoint,
            ],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError as e:
        print(e.stdout)
        print(e.stderr)
        raise e
    with open(out_file, "wb") as f:
        f.write(res.stdout)


def parallel_compile_types(
    *, src_file: Path, types: List[str], prefix="_view"
):
    # We use lambdas to compile types because tuples give inconsistent results
    types = "(" + "->".join([f"({t})" for t in types]) + ")"

    raws = ligo_compile_type(src_file=src_file, typ=types)

    args = []
    while raws["prim"] == "lambda":
        args.append(raws["args"][0])
        raws = raws["args"][1]
    args.append(raws)

    return args


def parallel_compile_views(src_file: Path, views, prefix="_view"):
    rows = "; ".join(
        [f"x{i} = ({prefix + v[0]})" for (i, v) in enumerate(views)]
    )
    types = "; ".join(
        [
            f"x{i} : (({v[1]} * {v[3]}) -> {v[2]})"
            for (i, v) in enumerate(views)
        ]
    )
    record_expr = "( { %(rows)s } : [@layout:comb] { %(types)s } )" % {
        "rows": rows,
        "types": types,
    }


    raw_json = ligo_compile_json(src_file=src_file, expr=record_expr)
    codes = json.loads(raw_json)
    # The compiler returns the views code under the following format:
    # [[view1, view2], view3, ...]
    return codes["args"]


def parallel_compile_entrypoints(main_file: Path, entrypoints):
    prefix = "Bytes.pack lazy_fun_"
    rows = "; ".join(
        [f"x{i} = {prefix + ent_name}" for (i, ent_name) in entrypoints]
    )
    types = "; ".join([f"x{i} : bytes" for (i, _) in entrypoints])
    record_expr = "({ %(rows)s } : [@layout:comb] { %(types)s })" % {
        "rows": rows,
        "types": types,
    }
    bytes = ligo_compile_json(src_file=main_file, expr=record_expr)
    return json.loads(bytes)


def compile_views(
    *, main_file: Path, views_file: Path, pattern: str, prefix="view_"
):
    views = find_views(views_file, pattern)
    packed_views = []

    args = parallel_compile_types(
        src_file=main_file, types=[v[1] for v in views]
    )
    returns = parallel_compile_types(
        src_file=main_file, types=[v[2] for v in views]
    )
    codes = parallel_compile_views(
        src_file=main_file, views=views, prefix=prefix
    )

    packed_views = []
    for view, code, arg_type, ret_type in zip(views, codes, args, returns):
        packed_views.append(
            {
                "name": view[0],
                "parameter": arg_type,
                "returnType": ret_type,
                "code": code,
            }
        )
    return {"views": packed_views}


def compile_entrypoints(*, main_file: Path, entrypoints_file: Path):
    chunk_size = 32000
    entrypoints = find_entrypoints(
        entrypoints_file, view_pattern=CHECKER_ENTRYPOINTS_PAT
    )

    json_bytes = parallel_compile_entrypoints(main_file, entrypoints)["args"]
    assert len(json_bytes) == len(entrypoints)

    packed_entrypoints = []
    for ent_id, ent_name in entrypoints:
        byts = json_bytes[ent_id]["bytes"]
        chunks = [
            byts[chunk_size * i : chunk_size * (i + 1)]
            for i in range(len(byts) // 32000 + 1)
        ]
        packed_entrypoints.append(
            {"name": ent_name, "fn_id": ent_id, "chunks": chunks}
        )
    return {"lazy_functions": packed_entrypoints}


def compile_checker(*, main_file: Path, entrypoints_file: Path):
    views = compile_views(
        main_file=main_file,
        views_file=entrypoints_file,
        pattern=CHECKER_VIEWS_PAT,
        prefix="wrapper_view_",
    )
    entrypoints = compile_entrypoints(
        main_file=main_file, entrypoints_file=entrypoints_file
    )
    views["lazy_functions"] = entrypoints["lazy_functions"]
    return views


def mockFA2_views(*, main_file: Path, views_file: Path):
    return compile_views(
        main_file=main_file, views_file=views_file, pattern=FA2_VIEWS_PAT
    )


def wtez_views(*, main_file: Path, views_file: Path):
    return compile_views(
        main_file=main_file, views_file=views_file, pattern=WTEZ_VIEWS_PAT
    )


def wctez_views(*, main_file: Path, views_file: Path):
    return compile_views(
        main_file=main_file, views_file=views_file, pattern=WCTEZ_VIEWS_PAT
    )


# TODO: use configurations
def compile_everything(*, out_dir:str="generated/michelson",
                       src_dir:str="src/",
                       vendor_dir:str="vendor/"):
    checkerMain = os.path.join(src_dir, "main.mligo")
    mockFA2Main = os.path.join(src_dir, "mockFA2Main.mligo")
    mockFA2Views = os.path.join(src_dir, "mockFA2.mligo")
    wtezMain = os.path.join(src_dir, "wtezMain.mligo")
    wtezViews = os.path.join(src_dir, "wtez.mligo")
    wctezMain = os.path.join(src_dir, "wctezMain.mligo")
    wctezViews = os.path.join(src_dir, "wctez.mligo")
    ctezMain = os.path.join(vendor_dir, "ctez/ctez.mligo")
    ctezCFMMMain = os.path.join(vendor_dir, "ctez/cfmm_tez_ctez.mligo")
    ctezFA12 = os.path.join(vendor_dir, "ctez/fa12.mligo")
    # FIXME add fa12

    if not os.path.exists(out_dir):
        os.makedirs(out_dir)

    checker_tz = os.path.join(out_dir, "main.tz")
    mockFA2_tz = os.path.join(out_dir, "mockFA2Main.tz")
    wtez_tz = os.path.join(out_dir, "wtezMain.tz")
    wctez_tz = os.path.join(out_dir, "wctezMain.tz")
    ctez_tz = os.path.join(out_dir, "ctez.tz")
    ctez_cfmm_tz = os.path.join(out_dir, "ctez_cfmm.tz")
    ctez_fa12_tz = os.path.join(out_dir, "ctez_fa12.tz")

    ligo_compile(src_file=checkerMain, entrypoint="main", out_file=checker_tz)
    ligo_compile(src_file=mockFA2Main, entrypoint="main", out_file=mockFA2_tz)
    ligo_compile(src_file=ctezMain, entrypoint="main", out_file=ctez_tz)
    ligo_compile(
        src_file=ctezCFMMMain, entrypoint="main", out_file=ctez_cfmm_tz
    )
    ligo_compile(src_file=wtezMain, entrypoint="main", out_file=wtez_tz)
    ligo_compile(src_file=wctezMain, entrypoint="main", out_file=wctez_tz)
    ligo_compile(src_file=ctezFA12, entrypoint="main", out_file=ctez_fa12_tz)

    # JSON files
    mockFA2_metadata = mockFA2_views(
        main_file=mockFA2Main, views_file=mockFA2Views
    )
    wtez_metadata = wtez_views(main_file=wtezMain, views_file=wtezViews)
    wctez_metadata = wctez_views(main_file=wctezMain, views_file=wctezViews)
    checker_functions = compile_checker(
        main_file=checkerMain, entrypoints_file=os.path.join(src_dir, "checkerEntrypoints.mligo")
    )
    with open(os.path.join(out_dir, "mock_fa2_metadata.json"), "w") as f:
        json.dump(mockFA2_metadata, f, indent=2)

    with open(os.path.join(out_dir, "wtez_metadata.json"), "w") as f:
        json.dump(wtez_metadata, f, indent=2)

    with open(os.path.join(out_dir, "wctez_metadata.json"), "w") as f:
        json.dump(wctez_metadata, f, indent=2)

    with open(os.path.join(out_dir, "functions.json"), "w") as f:
        json.dump(checker_functions, f, indent=2)

    return checker_functions, mockFA2_metadata, wtez_metadata
    # json.dumps(x, indent=2)
