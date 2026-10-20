# [[id:f682f4c1-f79d-426f-931d-851fdd081ef9][The server:3]]
import json
import pathlib

from qutebrowser.api import cmdutils, interceptor
from qutebrowser.commands import runners
from qutebrowser.completion.models import miscmodels
from qutebrowser.misc import objects
from qutebrowser.qt import sip
from qutebrowser.qt.core import QUrl
from qutebrowser.utils import objreg, usertypes

for name in ["agent-claim", "agent-former-eval", "agent-run", "agent-eval", "agent-requests", "agent-tabs", "agent-take"]:
    objects.commands.pop(name, None)

AGENT_TAB = {}


@cmdutils.register()
@cmdutils.argument("win_id", value=cmdutils.Value.win_id)
def agent_claim(win_id: int) -> None:
    """Make the tab this window shows the agent's, keeping the former one to take its 🤖 off."""
    tabbed = objreg.get("tabbed-browser", scope="window", window=win_id)
    AGENT_TAB.update(former=AGENT_TAB.get("tab"), win_id=win_id, tab=tabbed.widget.currentWidget())


@cmdutils.register()
def agent_former_eval(script: str, answer: str) -> None:
    """Run the javascript in SCRIPT in the agent's former tab, where it is still there and not its tab now.

    Write what it gives back to ANSWER, as JSON, null when there is no such tab."""
    def answered(value):
        part = pathlib.Path(f"{answer}.part")
        part.write_text(json.dumps(value))
        part.rename(answer)

    former = AGENT_TAB.get("former")
    if former is None or sip.isdeleted(former) or former is AGENT_TAB.get("tab"):
        answered(None)
        return
    former.run_js_async(pathlib.Path(script).read_text(), answered, world=usertypes.JsWorld.jseval)


def agent_tab():
    """The window and tab of the agent: the one pointed at, which bears the 🤖."""
    if not AGENT_TAB or AGENT_TAB["win_id"] not in objreg.window_registry or sip.isdeleted(AGENT_TAB["tab"]):
        raise cmdutils.CommandError("the agent has no tab of its own: open one with open_url_in_new_tab")
    tabbed = objreg.get("tabbed-browser", scope="window", window=AGENT_TAB["win_id"])
    if tabbed.widget.indexOf(AGENT_TAB["tab"]) < 0:
        raise cmdutils.CommandError("the agent's tab is gone: open one with open_url_in_new_tab")
    return tabbed, AGENT_TAB["win_id"], AGENT_TAB["tab"]


@cmdutils.register()
@cmdutils.argument("win_id", value=cmdutils.Value.win_id)
def agent_eval(script: str, answer: str, win_id: int) -> None:
    """Run the javascript in SCRIPT in the tab this window shows, writing what it gives back to ANSWER, as JSON."""
    tab = objreg.get("tabbed-browser", scope="window", window=win_id).widget.currentWidget()

    def answered(value):
        part = pathlib.Path(f"{answer}.part")
        part.write_text(json.dumps(value))
        part.rename(answer)

    tab.run_js_async(pathlib.Path(script).read_text(), answered, world=usertypes.JsWorld.jseval)


def page_of(url):
    return url.adjusted(QUrl.UrlFormattingOption.RemoveFragment).toString(QUrl.ComponentFormattingOption.FullyEncoded)


REQUESTED = interceptor.__dict__.setdefault("agent_requested", {})  # page → what it asked for, kept across re-sourcing


AWAITED = {interceptor.ResourceType.stylesheet, interceptor.ResourceType.script, interceptor.ResourceType.xhr}


def counted(request):
    page = page_of(request.first_party_url)
    if request.resource_type == interceptor.ResourceType.main_frame:
        REQUESTED[page] = []
    elif request.resource_type in AWAITED and not request.is_blocked:
        REQUESTED.setdefault(page, []).append(request.request_url.toString(QUrl.ComponentFormattingOption.FullyEncoded))


if not interceptor.__dict__.setdefault("agent_counting", False):
    interceptor.register(lambda request: counted(request))
    interceptor.agent_counting = True


@cmdutils.register()
@cmdutils.argument("win_id", value=cmdutils.Value.win_id)
def agent_requests(answer: str, win_id: int) -> None:
    """Write to ANSWER, as JSON, what the page this window shows asked for since it began loading."""
    tab = objreg.get("tabbed-browser", scope="window", window=win_id).widget.currentWidget()
    part = pathlib.Path(f"{answer}.part")
    part.write_text(json.dumps(REQUESTED.get(page_of(tab.url()), [])))
    part.rename(answer)


@cmdutils.register()
def agent_tabs(answer: str) -> None:
    """Write to ANSWER, as JSON, every tab: as WINDOW/INDEX, its title, its address, and whether it is the agent's."""
    tabs = []
    for win_id in list(objreg.window_registry):
        tabbed = objreg.get("tabbed-browser", scope="window", window=win_id)
        for index in range(tabbed.widget.count()):
            tab = tabbed.widget.widget(index)
            tabs.append({"tab": f"{win_id}/{index + 1}", "title": tab.title(), "url": tab.url().toDisplayString(),
                         "agent's": tab is AGENT_TAB.get("tab")})
    part = pathlib.Path(f"{answer}.part")
    part.write_text(json.dumps(tabs, ensure_ascii=False))
    part.rename(answer)


@cmdutils.register()
def agent_take(tab: str) -> None:
    """Make the tab TAB, as WINDOW/INDEX, the agent's."""
    win_id, index = (int(part) for part in tab.split("/"))
    if win_id not in objreg.window_registry:
        raise cmdutils.CommandError(f"no window {win_id}")
    widget = objreg.get("tabbed-browser", scope="window", window=win_id).widget.widget(index - 1)
    if widget is None:
        raise cmdutils.CommandError(f"no tab {tab}")
    AGENT_TAB.update(win_id=win_id, tab=widget)


@cmdutils.register(maxsplit=0)
def agent_run(command: str, *, answer: str = None) -> None:
    """Run COMMAND in the agent's tab, made current in its window, raising no window.

    When the agent has no tab, write why to ANSWER, as JSON, for whoever waits on it."""
    try:
        tabbed, win_id, tab = agent_tab()
    except cmdutils.CommandError as error:
        if answer:
            part = pathlib.Path(f"{answer}.part")
            part.write_text(json.dumps({"agent_run_failed": str(error)}))
            part.rename(answer)
        raise
    tabbed.widget.setCurrentWidget(tab)
    runners.CommandRunner(win_id).run_safely(command)
# The server:3 ends here
