# Setup the contracts on a Tezos network, typically Flextesa
import pytezos as tz
from checker_tools.client.operations import inject
from checker_tools.builder.config import CheckerRepo
from checker_tools.client import checker
import pytest
import requests
import urllib


# Used to avoid redeploying Checker when running integration tests.
# If checker code is modified, this needs to change as well.
CHECKER_TYPE_HASH = 1323991759
TZKT_API_URL = "http://127.0.0.1:5010"  # See docker-compose.yml
TEZOS_RPC = "http://localhost:20000"


alice = tz.Key.from_encoded_key(
    "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
)
ptz = tz.pytezos.using(TEZOS_RPC, alice)


def do(call, amount=None):
    """Calls an entrypoint, optionally sending an amount of mutez"""
    if amount is not None:
        call = call.with_amount(amount)
    tx = call.as_transaction()
    inject(ptz, tx.autofill(ttl=None).sign())


def add_operator_fa2(owner, address, token, token_id):
    return do(
        token.update_operators(
            [
                {
                    "add_operator": {
                        "owner": owner.public_key_hash(),
                        "operator": address,
                        "token_id": token_id,
                    }
                }
            ]
        )
    )


@pytest.fixture
def conf():
    conf = CheckerRepo(".")
    return conf


@pytest.fixture
def contracts(conf):
    try:
        print("*********************")
        api_url = urllib.parse.urljoin(
            TZKT_API_URL, f"v1/contracts?typeHash={CHECKER_TYPE_HASH}"
        )
        response = requests.get(api_url).json()
        if len(response) > 1:
            print("*** several contracts found")
        ch = ptz.contract(response[-1]["address"])
        print(f"*** Contract found at address {ch.address}")
        # Recover all the other contracts from this
        t = ch.storage()["deployment_state"]["sealed"]["external_contracts"]
        wtez = ptz.contract(t["collateral_fa2"])
        ctez_cfmm = ptz.contract(t["ctez_cfmm"])
        oracle = ptz.contract(t["oracle"])
        wctez = ptz.contract(t["ctok_fa2"])
        ctez = ptz.contract(ctez_cfmm.storage()["ctez_address"])
        fa12_ctez = ptz.contract(ctez_cfmm.storage()["cashAddress"])
        ctez_lqt = ptz.contract(ctez_cfmm.storage()["lqtAddress"])
    except:
        ctez_contracts = checker.deploy_ctez(ptz, conf)
        ctez = ctez_contracts["ctez"]
        fa12_ctez = ctez_contracts["fa12_ctez"]
        ctez_lqt = ctez_contracts["fa12_lqt"]
        ctez_cfmm = ctez_contracts["cfmm"]
        oracle = checker.deploy_contract(
            ptz,
            source_file="./utils/mock_oracle.tz",
            initial_storage={
                "owner": alice.public_key_hash(),
                "price": (1, 1),
            },
        )
        wtez = checker.deploy_wtez(ptz, conf)
        wctez = checker.deploy_wctez(
            ptz, conf, ctez_fa12_address=fa12_ctez.address
        )
        ch = checker.deploy_checker(
            ptz,
            conf,
            oracle=oracle,
            collateral_fa2=wtez,
            cfmm_token_fa2=wctez,
            ctez_cfmm=ctez_cfmm,
        )
    return {
        "ctez": ctez,
        "ctez_cfmm": ctez_cfmm,
        "fa12_ctez": fa12_ctez,
        "ctez_cfmm_lqt": ctez_lqt,
        "wtez": wtez,
        "wctez": wctez,
        "oracle": oracle,
        "checker": ch,
    }


@pytest.fixture
def alice_oven(contracts):
    ovens_id = contracts["ctez"].storage()["ovens"]
    ovens = ptz.shell.head.context.big_maps[ovens_id]()
    my_oven = ptz.contract(ovens[0]["args"][0]["args"][0]["string"])
    do(my_oven.default(), amount=(int(1e9)))
    do(contracts["ctez"].mint_or_burn({"id": 0, "quantity": int(5e8)}))
    return my_oven


@pytest.fixture
def alice_burrow(contracts):
    # FIXME: get token id from configuration
    ch = contracts["checker"]
    wtez = contracts["wtez"]
    do(wtez.deposit(), amount=int(1e8))
    add_operator_fa2(alice, ch.address, wtez, 2)
    # Should we use a view to return a new burrow each time?
    try:
        burrows = ch.storage["deployment_state"]["sealed"]["burrows"]
        burrows[(alice.public_key_hash(), 0)]()
    except KeyError:
        do(ch.create_burrow((0, None, int(1e8))))
    do(wtez.deposit(), amount=int(1e9))
    do(ch.deposit_collateral(0, int(1e9)))


def test_checker_deployed(contracts):
    ch = contracts["checker"]
    print(f"Checker deployed at address {ch.address}")
    assert "sealed" in ch.storage()["deployment_state"]


def test_mint_kit(contracts, alice_burrow):
    # FIXME: get fminting from config
    kit_amount = int((10 / 21) * 1e9)
    ch = contracts["checker"]
    kit_ledger = ch.storage["deployment_state"]["sealed"]["fa2_state"][
        "ledger"
    ]
    try:
        previous_kits = kit_ledger[(0, alice.public_key_hash())]()
    except KeyError:
        previous_kits = 0
    do(contracts["checker"].mint_kit(0, kit_amount))
    assert kit_ledger[(0, alice.public_key_hash())]() == (
        kit_amount + previous_kits
    )
