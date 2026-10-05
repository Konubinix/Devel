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

for name in ["agent-claim", "agent-run", "agent-eval", "agent-requests"]:
    objects.commands.pop(name, None)

AGENT_TAB = {}


@cmdutils.register()
@cmdutils.argument("win_id", value=cmdutils.Value.win_id)
def agent_claim(win_id: int) -> None:
    """Make the tab this window shows the agent's."""
    tabbed = objreg.get("tabbed-browser", scope="window", window=win_id)
    AGENT_TAB.update(win_id=win_id, tab=tabbed.widget.currentWidget())


def agent_tab():
    """The window and tab of the agent: the one claimed, else the one titled 🤖, as when given to another window."""
    if AGENT_TAB and AGENT_TAB["win_id"] in objreg.window_registry and not sip.isdeleted(AGENT_TAB["tab"]):
        tabbed = objreg.get("tabbed-browser", scope="window", window=AGENT_TAB["win_id"])
        if tabbed.widget.indexOf(AGENT_TAB["tab"]) >= 0:
            return tabbed, AGENT_TAB["win_id"], AGENT_TAB["tab"]
    tabs = miscmodels.tabs()
    tabs.set_pattern("🤖")
    if not tabs.count():
        raise cmdutils.CommandError("the agent has no tab of its own")
    win_id, index = (int(part) for part in tabs.data(tabs.first_item()).split("/"))
    tabbed = objreg.get("tabbed-browser", scope="window", window=win_id)
    AGENT_TAB.update(win_id=win_id, tab=tabbed.widget.widget(index - 1))
    return tabbed, win_id, AGENT_TAB["tab"]


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


@cmdutils.register(maxsplit=0)
def agent_run(command: str) -> None:
    """Run COMMAND in the agent's tab, made current in its window, raising no window."""
    tabbed, win_id, tab = agent_tab()
    tabbed.widget.setCurrentWidget(tab)
    runners.CommandRunner(win_id).run_safely(command)
# The server:3 ends here
