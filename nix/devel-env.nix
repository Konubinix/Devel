# Environment variables from the devel repository.
# Replaces the devel-specific parts of config/env.conf.
#
# Perso-dependent vars (KONIX_PERSO_DIR, HOSTNAME) belong in perso-env.nix
# and host-env.nix respectively.
{ config, ... }:

let
  homeDir = config.home.homeDirectory;
  develDir = "/home/sam/prog/devel";
  configDir = "${develDir}/config";
  shareDir = "${develDir}/share";
in
{
  home.sessionVariables = {
    # Bootstrap paths
    KONIX_DEVEL_DIR = develDir;
    KONIX_CONFIG_DIR = configDir;
    KONIX_SHARE_DIR = shareDir;
    KONIX_LIB_DIR = "${develDir}/lib";

    # Editors
    EDITOR = "edit.sh";
    VISUAL = "edit.sh";
    ALTERNATE_EDITOR = "edaemon";
    SVN_EDITOR = "nano";
    BROWSER = "qutebrowser";

    # Locale
    LANG = "C.UTF-8";
    LC_ALL = "C.UTF-8";

    # Shell / history
    HISTSIZE = "5000000";
    HISTFILESIZE = "5000000";
    HISTTIMEFORMAT = "%y/%m/%d-%H:%M:%S:";
    HISTCONTROL = "ignoreboth";
    FZF_CTRL_R_OPTS = "--exact";
    BKT_TTL = "3m";
    LESS = "-SRMX";
    LESSOPEN = "|source-highlight-esc.sh %s";

    # Dirs
    TMPDIR = "${homeDir}/tmp";
    KONIX_SOB_DIR = "${homeDir}/tmp/sob";
    KONIX_TMP_SHARE_DIR = "${homeDir}/tmp/share";
    KONIX_DOWNLOAD_DIR = "${homeDir}/Downloads";
    KONIX_CRYPTOFF = "${homeDir}/cryptoff";
    KONIX_SSHFS_ROOT = "${homeDir}/machines";
    KONIX_SMB_MOUNT = "${homeDir}/Network";
    KONIX_WATCHER_CTRL_DIR = "${homeDir}/ctrl";
    KONIX_LOG_DIR = "${homeDir}/log";
    KONIX_LOG_ERR_DIR = "${homeDir}/log/err";
    KONIX_REMOTE_QUIET = "1";

    # XDG (devel-only; perso-dependent XDG vars go in perso-env.nix)
    XDG_CONFIG_HOME = configDir;
    XDG_CACHE_HOME = "${homeDir}/.cache";
    XDG_DOWNLOAD_DIR = "${homeDir}/Downloads";
    XDG_LOG_HOME = "${homeDir}/tmp/logs";

    # Build tools
    KONIX_MAKE = "konix_makej.sh";
    KONIX_MAKE_LOAD_LIMIT = "auto";
    KONIX_MAKE_SUDIRS = "build:Build";
    KONIX_CCACHE = "ccache";
    KONIX_CC = "gcc";
    KONIX_CXX = "g++";
    CC = "konix_cc.sh";
    CXX = "konix_cxx.sh";

    # Python
    PYTHONBREAKPOINT = "ipdb.set_trace";
    PYTHONSTARTUP = "";
    VIRTUALENVWRAPPER_PYTHON = "python3";

    # Go
    GO111MODULE = "auto";
    GOPATH = "${homeDir}/gopath";

    # Rust
    RUST_BACKTRACE = "full";

    # Node
    PNPM_HOME = "${homeDir}/.pnpm-home";
    N_PREFIX = "${homeDir}/n";
    npm_config_prefix = "${homeDir}/.local";
    NODE_PATH = "${homeDir}/.local/lib/node_modules:${homeDir}/.pnpm-home/global/5/node_modules";

    # Ruby
    GEM_HOME = "${homeDir}/.local";

    # Git
    GIT_ALLOW_PROTOCOL = "file:git:http:https:ssh:bzr";
    KONIX_GIT_ANNEX_DIRED_METADATA = "--metadata state=next";
    KONIX_GIT_ANNEX_DIRED_SCAN_DIRECTORY = "1";

    # Config file refs (devel)
    MAILCAPS = "${configDir}/mailcap";
    HYPERMAIL_CONFIG = "${configDir}/hmrc";
    KONIX_SUPERVISORDCONF = "${configDir}/supervisord.conf";
    KONIX_UDISKIE_CONFIG = "${configDir}/udiskie.yml";
    KONIX_DEFAULT_CTAGS_CONFIG = "${configDir}/ctags";
    MERGE_ICS_CONF_FILE = "${configDir}/merge_ics.cfg";
    XCOMPOSEFILE = "${configDir}/XCompose";
    KONIX_CONKYRC = "${configDir}/conky/conkyrc";

    # Mail
    KONIX_MAIL = "konubinixweb@gmail.com";
    KONIX_OFFLINEIMAP_FOLDERS_WITH_TRASH = "0";
    KONIX_MSMTP_LOG = "-";
    KONIX_TEMP_MAIL_KEEP_MONTHS = "3";

    # Network ports / services
    KONIX_HTTP_SHARE_PORT = "9643";
    KONIX_FTP_SHARE_PORT = "9645";
    KONIX_HTTP_SHARE_DIRECTORY_PORT = "9642";
    KONIX_FTP_SHARE_DIRECTORY_PORT = "9644";
    KONIX_NOTEBOOK_PORT = "9640";
    KONIX_NOTEBOOK_IP = "127.0.0.1";
    KONIX_NBVIEWER_PORT = "9638";
    KONIX_NBVIEWER_IP = "0.0.0.0";
    KONIX_NOTEBOOK_BROWSER = "chromium";
    KONIX_NOTEBOOK_VIRTUALENV = "maths3";
    KONIX_MINIDNS_PORT = "53000";
    PROXYRESOLV_DNS = "127.0.0.1 -p53000";
    KONIX_BEETWEB_EXTRA_ARGUMENTS = "127.0.0.1 8337";
    KONIX_GDBSERVER_CONNECT = "localhost:9999";
    KONIX_LANGTOOL_JAR = "${homeDir}/local/languagetool-commandline.jar";

    # Xapers
    KONIX_XAPERS_ADD_TAGS = "new";
    KONIX_XAPERS_ADD_AUTO_PROMPT = "1";
    KONIX_XAPERS_ADD_AUTO_VIEW = "";
    KONIX_XAPERS_ADD_AUTO_MOVE = "1";

    # Syncplay
    KONIX_SYNCPLAY_SERVER = "192.168.1.2";
    KONIX_SYNCPLAY_ROOM = "room";
    KONIX_SYNCPLAY_EXTRA_CONFIG = "-input file=${homeDir}/.mplayer/fifo";

    # GTK
    GTK_IM_MODULE = "xim";

    # Misc
    wpsetters = "feh";
    QUILT_PATCHES = "patches";
    MANPATH = "${homeDir}/.nix-profile/share/man:";
    LD_LIBRARY_PATH = "${homeDir}/.local/lib";
    PKG_CONFIG_PATH = "";
    KONIX_RIL_TRIES = "3";
    KONIX_RIL_TIMEOUT = "10";

    # Bash completion
    COMP_IWLIST_SCAN = "1";
    COMP_CONFIGURE_HINTS = "1";
    COMP_KNOWN_HOSTS_WITH_HOSTFILE = "1";
    COMP_KNOWN_HOSTS_WITH_AVAHI = "1";
    COMP_TAR_INTERNAL_PATHS = "1";
  };
}
