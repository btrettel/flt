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

# <https://stackoverflow.com/a/61569783/1124489>
def datetime_valid(dt_str):
    try:
        datetime.fromisoformat(dt_str)
    except:
        return False
    return True

class log_lines(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        with open("logging.jsonl") as jsonl_file_handler:
            line = jsonl_file_handler.readline()
            
            while line:
                data = json.loads(line)
                
                if data["message"] == "log_message test":
                    cls.log_message_present = True
                    cls.log_message_rc      = int(data["rc"])
                
                if data["message"] == "ERROR: log_error test":
                    cls.log_error_present = True
                    cls.log_error_rc      = int(data["rc"])
                
                if data["message"] == "dict_log(0) test":
                    cls.dict_log_0_present = True
                    cls.dict_log_0_rc      = int(data["rc"])
                    cls.dict_log_0_len     = len(data.keys())
                
                if data["message"] == "ERROR: dict_log(0) test (error)":
                    cls.dict_log_0_error_present = True
                    cls.dict_log_0_error_rc      = int(data["rc"])
                    cls.dict_log_0_error_len     = len(data.keys())
                
                if data["message"] == "dict_log(1) test":
                    cls.dict_log_1_present = True
                    cls.dict_log_1_rc      = int(data["rc"])
                    cls.dict_log_1_integer = int(data["test_integer"])
                    cls.dict_log_1_len     = len(data.keys())
                
                if data["message"] == "ERROR: dict_log(1) test (error)":
                    cls.dict_log_1_error_present = True
                    cls.dict_log_1_error_rc      = int(data["rc"])
                    cls.dict_log_1_error_integer = int(data["test_integer"])
                    cls.dict_log_1_error_len     = len(data.keys())
                
                if data["message"] == "dict_log(2) test":
                    cls.dict_log_2_present   = True
                    cls.dict_log_2_rc        = int(data["rc"])
                    cls.dict_log_2_integer_1 = int(data["test_integer_1"])
                    cls.dict_log_2_integer_2 = int(data["test_integer_2"])
                    cls.dict_log_2_len       = len(data.keys())
                
                if data["message"] == "ERROR: dict_log(2) test (error)":
                    cls.dict_log_2_error_present   = True
                    cls.dict_log_2_error_rc        = int(data["rc"])
                    cls.dict_log_2_error_integer_1 = int(data["test_integer_1"])
                    cls.dict_log_2_error_integer_2 = int(data["test_integer_2"])
                    cls.dict_log_2_error_len       = len(data.keys())
                
                if data["message"] == "custom rc test":
                    cls.custom_rc_present = True
                    cls.custom_rc_rc      = int(data["rc"])
                
                if data["message"] == "ERROR: custom rc test (error)":
                    cls.custom_rc_error_present = True
                    cls.custom_rc_error_rc      = int(data["rc"])
                
                # Advance line
                line = jsonl_file_handler.readline()
    
    def test_log_message(self):
        self.assertTrue(log_lines.log_message_present)
        self.assertEqual(log_lines.log_message_rc, 0)
    
    def test_log_error(self):
        self.assertTrue(log_lines.log_error_present)
        self.assertEqual(log_lines.log_error_rc, 1)
    
    def test_log_dict_log_0(self):
        self.assertTrue(log_lines.dict_log_0_present)
        self.assertEqual(log_lines.dict_log_0_rc, 0)
        self.assertEqual(log_lines.dict_log_0_len, 3)
    
    def test_log_dict_log_0_error(self):
        self.assertTrue(log_lines.dict_log_0_error_present)
        self.assertEqual(log_lines.dict_log_0_error_rc, 1)
        self.assertEqual(log_lines.dict_log_0_error_len, 3)
    
    def test_log_dict_log_1(self):
        self.assertTrue(log_lines.dict_log_1_present)
        self.assertEqual(log_lines.dict_log_1_rc, 0)
        self.assertEqual(log_lines.dict_log_1_integer, 1234)
        self.assertEqual(log_lines.dict_log_1_len, 4)
    
    def test_log_dict_log_1_error(self):
        self.assertTrue(log_lines.dict_log_1_error_present)
        self.assertEqual(log_lines.dict_log_1_error_rc, 1)
        self.assertEqual(log_lines.dict_log_1_error_integer, 9876)
        self.assertEqual(log_lines.dict_log_1_error_len, 4)
    
    def test_log_dict_log_2(self):
        self.assertTrue(log_lines.dict_log_2_present)
        self.assertEqual(log_lines.dict_log_2_rc, 0)
        self.assertEqual(log_lines.dict_log_2_integer_1, 3456)
        self.assertEqual(log_lines.dict_log_2_integer_2, 890)
        self.assertEqual(log_lines.dict_log_2_len, 5)
    
    def test_log_dict_log_2_error(self):
        self.assertTrue(log_lines.dict_log_2_error_present)
        self.assertEqual(log_lines.dict_log_2_error_rc, 1)
        self.assertEqual(log_lines.dict_log_2_error_integer_1, 987)
        self.assertEqual(log_lines.dict_log_2_error_integer_2, 5432)
        self.assertEqual(log_lines.dict_log_2_error_len, 5)
    
    def test_log_custom_rc(self):
        self.assertTrue(log_lines.custom_rc_present)
        self.assertEqual(log_lines.custom_rc_rc, 1234)
    
    def test_log_custom_rc_error(self):
        self.assertTrue(log_lines.custom_rc_error_present)
        self.assertEqual(log_lines.custom_rc_error_rc, 5678)

class log_other(unittest.TestCase):
    def test_time(self):
        with open("logging.jsonl") as jsonl_file_handler:
            line = jsonl_file_handler.readline()
            while line:
                data = json.loads(line)
                
                # Tests on the date in the logs is valid.
                self.assertTrue(len(data["time"]) == 29)
                self.assertTrue(not " " in data["time"])
                self.assertTrue(datetime_valid(data["time"]))
                
                # Advance line
                line = jsonl_file_handler.readline()

if __name__ == "__main__":
    #unittest.main(verbosity=2)
    unittest.main()
