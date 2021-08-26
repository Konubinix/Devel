#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from odo import odo

def odo_sql_circular_foreign_keys_workaround():
    import sqlalchemy as sa
    from datashape import discover
    @discover.register(sa.ForeignKey, sa.sql.FromClause)
    def do_not_discover_foreign_key_relationship(fk, parent, parent_measure=None):
        return {}

    import odo.backends.sql
    odo.backends.sql.discover_foreign_key_relationship = do_not_discover_foreign_key_relationship
    print("You can now load databases with circular foreign keys")
