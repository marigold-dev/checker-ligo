import subprocess
from pathlib import Path
import re
import json
from typing import List, Optional, Tuple

FA2_VIEWS_PAT = (
    r"let view_(\S+) *\([^:]*: *(.*) \* mock_fa2_state\) *: *([^=]*)"
)
WTEZ_VIEWS_PAT = r"let view_(\S+) *\([^:]*: *(.*) \* wtez_state\) *: *([^=]*)"
WCTEZ_VIEWS_PAT = (
    r"let view_(\S+) *\([^:]*: *(.*) \* wctez_state\) *: *([^=]*)"
)
CHECKER_VIEWS_PAT = (
    r"let wrapper_view_(\S+) *\([^:]*: *(.*) \* wrapper\) *: *([^=]*)"
)
CHECKER_ENTRYPOINTS_PAT = r"let lazy_id_(\S+) *= \(*(\d*)\)"


def find_views(
    src_file: Path, view_pattern: str
) -> List[Tuple[str, str, str]]:
    with open(src_file) as f:
        code = f.read()
        raw_views = re.findall(view_pattern, code)
        return [
            (name, arg_type.strip(), ret_type.strip())
            for (name, arg_type, ret_type) in raw_views
        ]


def find_entrypoints(
    src_file: Path, view_pattern: str
) -> List[Tuple[str, int]]:
    with open(src_file) as f:
        code = f.read()
        raw_views = re.findall(view_pattern, code)
        return [(name, int(id)) for (name, id) in raw_views]


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


def compile_views(*, main_file: Path, views_file: Path, pattern: str,
                  prefix="view_"):
    views = find_views(views_file, pattern)
    packed_views = []
    print(f"Found {len(views)} views to compile")
    for name, arg_type, ret_type in views:
        print(f"Compiling view {prefix+name}")
        view = ligo_compile_json(main_file, prefix + name)
        code = json.loads(view)
        arg_type = ligo_compile_type(src_file=main_file, typ=arg_type)
        ret_type = ligo_compile_type(src_file=main_file, typ=ret_type)
        packed_views.append(
            {
                "name": name,
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
    print(f"Found {len(entrypoints)} entrypoints to compile")
    packed_entrypoints = []
    for ent_name, ent_id in entrypoints:
        print(f"Compiling entrypoint lazy_fun_{ent_name}")
        expr = f"Bytes.pack lazy_fun_{ent_name}"
        byts = json.loads(ligo_compile_json(src_file=main_file, expr=expr))[
            "bytes"
        ]

        chunks = [
            byts[chunk_size * i : chunk_size * (i + 1)]
            for i in range(len(byts) // 32000 + 1)
        ]
        packed_entrypoints.append(
            {"name": ent_name, "fn_id": ent_id, "chunks": chunks}
        )
    return {"lazy_functions": packed_entrypoints}


def compile_checker(*, main_file: Path, entrypoints_file: Path):
    views = compile_views(main_file=main_file, views_file=entrypoints_file,
                          pattern=CHECKER_VIEWS_PAT, prefix="wrapper_view_")
    entrypoints = compile_entrypoints(main_file=main_file,
                                      entrypoints_file=entrypoints_file)
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


def compile_everything():
    checkerMain = "src/main.mligo"
    mockFA2Main = "src/mockFA2Main.mligo"
    mockFA2Views = "src/mockFA2.mligo"
    wtezMain = "src/wtezMain.mligo"
    wtezViews = "src/wtez.mligo"

    ligo_compile(src_file=checkerMain, entrypoint="main", out_file="FIXME")
    ligo_compile(src_file=mockFA2Main, entrypoint="main", out_file="FIXME")
    ligo_compile(src_file=wtezMain, entrypoint="main", out_file="FIXME")
    mockFA2_metadata = mockFA2_views(
        main_file=mockFA2Main, views_file=mockFA2Views
    )
    wtez_metadata = wtez_views(main_file=wtezMain, views_file=wtezViews)

    return mockFA2_metadata, wtez_metadata
    # json.dumps(x, indent=2)