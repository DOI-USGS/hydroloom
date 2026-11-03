"""Error and warning classes and the catalog helper (SPEC A-10, A-11)."""

import builtins
import json
import string
from importlib.resources import files

import pytest

import hydroloom
from hydroloom import errors
from hydroloom.errors import (
    CRSError,
    HydroloomError,
    HydroloomWarning,
    RepresentationError,
    msg,
)

CATALOG = json.loads(
    files("hydroloom.data").joinpath("remedies.json").read_text(encoding="utf-8")
)["remedies"]


def _fields(template):
    return {name for _, name, _, _ in string.Formatter().parse(template) if name}


@pytest.mark.parametrize("key", sorted(CATALOG))
def test_every_key_formats_with_its_placeholders(key):
    """Each entry formats from its declared placeholders and uses all of them."""
    entry = CATALOG[key]
    declared = set(entry["placeholders"])
    values = {name: f"<{name}>" for name in declared}

    text = msg(key, **values)

    assert "{" not in text and "}" not in text
    for name in declared:
        assert f"<{name}>" in text
    for field in ("message", "message_python"):
        if field in entry:
            assert _fields(entry[field]) == declared


def test_message_python_preferred():
    """The Python text wins over the shared text where an entry has both."""
    entry = CATALOG["start_not_in_network"]

    assert msg("start_not_in_network") == entry["message_python"]
    assert msg("network_cycle") == CATALOG["network_cycle"]["message"]


def test_unknown_key_raises_with_key():
    """An unknown key is a developer guard: a built-in error naming it (B-2)."""
    with pytest.raises(KeyError, match="no_such_key") as info:
        msg("no_such_key")

    assert not isinstance(info.value, HydroloomError)


def test_missing_placeholder_raises_with_key_and_name():
    """A missing placeholder names the key and the placeholder (B-2)."""
    with pytest.raises(KeyError, match=r"needs_topo\.") as info:
        msg("needs_topo", function="sort_network")

    assert "current" in str(info.value)
    assert not isinstance(info.value, HydroloomError)


def test_class_hierarchy():
    """Three error classes, each a ValueError; one warning class (A-10, A-11)."""
    assert issubclass(HydroloomError, ValueError)
    for cls in (RepresentationError, CRSError):
        assert issubclass(cls, HydroloomError)
        assert issubclass(cls, ValueError)
    assert issubclass(HydroloomWarning, Warning)
    assert not issubclass(HydroloomWarning, HydroloomError)

    defined = {
        name
        for name, obj in vars(errors).items()
        if isinstance(obj, type)
        and issubclass(obj, BaseException)
        and obj.__module__ == errors.__name__
    }
    assert defined == {
        "HydroloomError",
        "RepresentationError",
        "CRSError",
        "HydroloomWarning",
    }


def test_catalog_python_classes_exist():
    """Every python_class is a built-in or errors class that matches its kind."""
    for key, entry in CATALOG.items():
        name = entry.get("python_class")
        if name is None:
            continue
        cls = getattr(errors, name, None) or getattr(builtins, name)
        assert isinstance(cls, type), key
        if entry["kind"] == "warning":
            assert issubclass(cls, Warning), key
        else:
            assert issubclass(cls, Exception), key
            assert not issubclass(cls, Warning), key


def test_not_in_all():
    """The classes are importable from hydroloom.errors, not exported (A-18)."""
    for name in ("HydroloomError", "RepresentationError", "CRSError", "msg"):
        assert name not in hydroloom.__all__


def test_stack_level_from_outside_the_package_is_one():
    """Called from outside hydroloom, the level is 1: the caller (A-11)."""
    assert errors._stack_level() == 1
