PROJ_DIR = ~/Prog

PROJ_MAKEFILE = $(PROJ_DIR)/Makefile

PROJ_CMD = cd $(PROJ_DIR) && $(MAKE) -f "$(PROJ_MAKEFILE)"

.PHONY : start all re rere run nrun kill

# ************************************************************
# GENERIC
# ************************************************************
start:
	$(PROJ_CMD)

all:
	$(PROJ_CMD) all

re:
	$(PROJ_CMD) re

rere:
	$(PROJ_CMD) rere

nrun:
	$(PROJ_CMD) nrun

run:
	$(PROJ_CMD) run

kill:
	$(PROJ_CMD) kill

clean:
	$(PROJ_CMD) clean

clean_all:
	$(PROJ_CMD) clean_all

# ************************************************************
# CUSTO
# ************************************************************
vk:
	$(PROJ_CMD) vk
