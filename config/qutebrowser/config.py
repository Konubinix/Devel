#!/usr/bin/env python
# -*- coding:utf-8 -*-

# c.tabs.position = "left"
# c.completion.shrink = True
config.unbind('d', mode='normal')
config.bind('dsd', 'spawn -u konix_qutebrowser_web_search.sh -dc')
config.bind('yas', 'spawn -u konix_qutebrowser_ril_save_url.sh')
config.bind('dsD', 'spawn -u konix_qutebrowser_web_search.sh -d')
config.bind('dsS', 'spawn -u konix_qutebrowser_web_search.sh')
config.bind('dss', 'spawn -u konix_qutebrowser_web_search.sh -c')
config.bind('ye', 'spawn -u konix_qutebrowser_edit_emacs.sh -c')
config.bind('dd', 'tab-close')
config.bind('zc', 'spawn -u konix_qutebrowser_open_with_chromium.sh')
config.bind('zf', 'spawn -u konix_qutebrowser_open_with_firefox.sh')
config.bind('Uc', 'set network user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.143 Safari/537.36"')
config.bind('Uf', 'set network user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0"')
config.bind('Ud', 'set network user-agent ""')
config.bind("sp", "spawn -u konix_qutebrowser_password.sh")
