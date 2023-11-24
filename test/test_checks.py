#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

# # $File$
# 
# Summary: tests logging.jsonl
# Author: Ben Trettel (<http://trettel.us/>)
# Last updated: $Date$
# Revision: $Revision$
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

from datetime import datetime
import unittest
import json

class log_lines(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        with open("asserts.jsonl") as jsonl_file_handler:
            line = jsonl_file_handler.readline()
            
            while line:
                data = json.loads(line)
                
                if data["message"] == "ERROR: check, .false.":
                    cls.check_false_rc = int(data["rc"])
                
                if data["message"] == "ERROR: check, .false., dict_log":
                    cls.check_false_dict_integer = int(data["check_integer"])
                
                # Advance line
                line = jsonl_file_handler.readline()
    
    def test_log_check_false(self):
        self.assertEqual(log_lines.check_false_rc, 1)
        self.assertEqual(log_lines.check_false_dict_integer, 67890)

if __name__ == "__main__":
    #unittest.main(verbosity=2)
    unittest.main()
