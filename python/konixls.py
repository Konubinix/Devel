#!/usr/bin/env python3
# -*- coding:utf-8 -*-

"""Bunch of helpers to ease the manipulation of xlrd sheets."""

import itertools

def base26_to_decimal(value):
    if len(value) == 1:
        return ord(value.lower()) - ord("a") + 1
    else:
        return base26_to_decimal(value[-1]) \
            + 26 * base26_to_decimal(value[:-1])

def decimal_to_base26(value):
    assert value>0
    value = value - 1
    d, m = divmod(value, 26)
    res = chr(m + ord("a"))
    if d:
        return decimal_to_base26(d) + res
    else:
        return res

def get_all_sheets(workbook):
    return [workbook.sheet_by_name(name)
            for name in workbook.sheet_names()]

def fix_indices(rows, cols):
  if type(rows) == int:
    rows = (rows, rows)
  if type(cols) == str:
    cols = (cols, cols)
  rows = (
    rows[0] - 1,
    rows[1],
  )
  cols = (
      base26_to_decimal(cols[0]) - 1,
      base26_to_decimal(cols[1]),
  )
  return (rows, cols)

def unfix_indices(rows, cols):
  if type(rows) == int:
    rows = (rows, rows)
  if type(cols) == int:
    cols = (cols, cols)
  rows = (
    rows[0] + 1,
    rows[1],
  )
  cols = (
      decimal_to_base26(cols[0]),
      decimal_to_base26(cols[1] - 1),
  )
  return (rows, cols)

def get_indices(rows, cols):
  rows, cols = fix_indices(rows, cols)
  return itertools.product(range(*rows), range(*cols))

def get_values(page, rows, cols):
    return [page.row_values(coord[0])[coord[1]]
            for coord in get_indices(rows, cols)
        ]

def get_values_axes(page, rows, cols):
    rows, cols = fix_indices(rows, cols)
    return [
        [
            page.row_values(row)[col]
            for col in xrange(*cols)
        ]
        for row in xrange(*rows)
    ]

def compute_expected_values(rows, cols, expected_values):
  expected_values_lines = [value.rstrip('\n') for value in
                           expected_values.splitlines()]
  _, fixed_cols = fix_indices(rows, cols)
  number_of_cols = fixed_cols[1] - fixed_cols[0]

  def adjust_cols(list_, number_of_cols):
      return list_ + [''] * (number_of_cols - len(list_))

  for expected_row in expected_values_lines:
    for expected_col in adjust_cols(expected_row.split("\t"), number_of_cols):
      yield expected_col

def sanity_check(page, rows, cols, expected_values):
  expected_values = compute_expected_values(rows, cols, expected_values)

  for index, expected_value, value in zip(get_indices(rows, cols),
                                   expected_values,
                                   get_values(page, rows, cols)):
    human_readable_values = unfix_indices(*index)
    assert expected_value == value, "in page %s, at %s, Expected '%s', got '%s'" % (
      page.number + 1,
      str(
        (
          human_readable_values[0][0],
          human_readable_values[1][0]
        )
      ),
      expected_value.encode("utf-8"),
      str(value))
