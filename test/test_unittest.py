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
        with open("unittest.jsonl") as jsonl_file_handler:
            line = jsonl_file_handler.readline()
            
            first_time_set = False
            
            while line:
                data = json.loads(line)
                
                if not first_time_set:
                    cls.first_time = data["time"]
                    first_time_set = True
                
                cls.last_rc      = data["rc"]
                cls.last_message = data["message"]
                cls.last_time    = data["time"]
                
                if data["message"] == "pass: logical_test, .true.":
                    cls.logical_test_pass_present = True
                    cls.logical_test_pass_rc      = int(data["rc"])
                    cls.logical_test_pass_type    = data["type"]
                
                if data["message"] == "fail: logical_test, failure":
                    cls.logical_test_fail_present  = True
                    cls.logical_test_fail_rc       = int(data["rc"])
                    cls.logical_test_fail_type     = data["type"]
                    cls.logical_test_fail_failures = int(data["cumulative failures"])
                
                if data["message"] == "pass: real_equality_test, identical numbers (1)":
                    cls.real_test_pass_present  = True
                    cls.real_test_pass_rc       = int(data["rc"])
                    cls.real_test_pass_type     = data["type"]
                    cls.real_test_pass_returned = float(data["real returned"])
                    cls.real_test_pass_expected = float(data["real expected"])
                    cls.real_test_pass_tol      = float(data["tolerance"])
                    cls.real_test_pass_diff     = float(data["difference"])
                
                if data["message"] == "fail: real_equality_test, failure (greater)":
                    cls.real_test_fail_present  = True
                    cls.real_test_fail_rc       = int(data["rc"])
                    cls.real_test_fail_type     = data["type"]
                    cls.real_test_fail_returned = float(data["real returned"])
                    cls.real_test_fail_expected = float(data["real expected"])
                    cls.real_test_fail_tol      = float(data["tolerance"])
                    cls.real_test_fail_diff     = float(data["difference"])
                
                if data["message"] == "pass: real_inequality_test, different numbers (1)":
                    cls.realne_test_pass_present  = True
                    cls.realne_test_pass_rc       = int(data["rc"])
                    cls.realne_test_pass_type     = data["type"]
                    cls.realne_test_pass_returned = float(data["real returned"])
                    cls.realne_test_pass_expected = float(data["real expected"])
                    cls.realne_test_pass_diff     = float(data["difference"])
                
                if data["message"] == "fail: real_inequality_test, failure":
                    cls.realne_test_fail_present  = True
                    cls.realne_test_fail_rc       = int(data["rc"])
                    cls.realne_test_fail_type     = data["type"]
                    cls.realne_test_fail_returned = float(data["real returned"])
                    cls.realne_test_fail_expected = float(data["real expected"])
                    cls.realne_test_fail_diff     = float(data["difference"])
                
                if data["message"] == "pass: integer_equality_test":
                    cls.integer_test_pass_present  = True
                    cls.integer_test_pass_rc       = int(data["rc"])
                    cls.integer_test_pass_type     = data["type"]
                    cls.integer_test_pass_returned = int(data["integer returned"])
                    cls.integer_test_pass_expected = int(data["integer expected"])
                    cls.integer_test_pass_diff     = int(data["difference"])
                
                if data["message"] == "fail: integer_equality_test, failure (greater)":
                    cls.integer_test_fail_present  = True
                    cls.integer_test_fail_rc       = int(data["rc"])
                    cls.integer_test_fail_type     = data["type"]
                    cls.integer_test_fail_returned = int(data["integer returned"])
                    cls.integer_test_fail_expected = int(data["integer expected"])
                    cls.integer_test_fail_diff     = int(data["difference"])
                
                if data["message"] == "pass: character_equality_test":
                    cls.string_test_present  = True
                    cls.string_test_rc       = int(data["rc"])
                    cls.string_test_type     = data["type"]
                    cls.string_test_returned = data["string returned"]
                    cls.string_test_expected = data["string expected"]
                
                if data["message"] == "fail: character_equality_test, failure (greater)":
                    cls.string_test_fail_present  = True
                    cls.string_test_fail_rc       = int(data["rc"])
                    cls.string_test_fail_type     = data["type"]
                    cls.string_test_fail_returned = data["string returned"]
                    cls.string_test_fail_expected = data["string expected"]
                
                if data["message"] == "All tests passed.":
                    cls.duration = float(data["duration (s)"])
                
                # Advance line
                line = jsonl_file_handler.readline()
    
    def test_log_logical_test(self):
        self.assertTrue(log_lines.logical_test_pass_present)
        self.assertEqual(log_lines.logical_test_pass_rc, 0)
        self.assertEqual(log_lines.logical_test_pass_type, "test")
    
    def test_log_logical_test_fail(self):
        self.assertTrue(log_lines.logical_test_fail_present)
        self.assertEqual(log_lines.logical_test_fail_rc, 1)
        self.assertEqual(log_lines.logical_test_fail_type, "test")
    
    def test_log_real_test(self):
        self.assertTrue(log_lines.real_test_pass_present)
        self.assertEqual(log_lines.real_test_pass_rc, 0)
        self.assertEqual(log_lines.real_test_pass_type, "test")
        self.assertEqual(log_lines.real_test_pass_returned, 1.)
        self.assertEqual(log_lines.real_test_pass_expected, 1.)
        self.assertGreater(log_lines.real_test_pass_tol, 0.)
        self.assertLess(log_lines.real_test_pass_tol, 1.e-9)
        self.assertAlmostEqual(log_lines.real_test_pass_diff, 0.)
    
    def test_log_real_test_fail(self):
        self.assertTrue(log_lines.real_test_fail_present)
        self.assertEqual(log_lines.real_test_fail_rc, 1)
        self.assertEqual(log_lines.real_test_fail_type, "test")
        self.assertEqual(log_lines.real_test_fail_returned, 1.)
        self.assertEqual(log_lines.real_test_fail_expected, 0.)
        self.assertGreater(log_lines.real_test_fail_tol, 0.)
        self.assertLess(log_lines.real_test_fail_tol, 1.e-9)
        self.assertNotAlmostEqual(log_lines.real_test_fail_diff, 0.)
        self.assertGreater(log_lines.real_test_fail_diff, 0.)
    
    def test_log_realne_test(self):
        self.assertTrue(log_lines.realne_test_pass_present)
        self.assertEqual(log_lines.realne_test_pass_rc, 0)
        self.assertEqual(log_lines.realne_test_pass_type, "test")
        self.assertEqual(log_lines.realne_test_pass_returned, 1.)
        self.assertEqual(log_lines.realne_test_pass_expected, 10.)
        self.assertNotAlmostEqual(log_lines.realne_test_pass_diff, 0.)
        self.assertGreater(log_lines.realne_test_pass_diff, 0.)
    
    def test_log_realne_test_fail(self):
        self.assertTrue(log_lines.realne_test_fail_present)
        self.assertEqual(log_lines.realne_test_fail_rc, 1)
        self.assertEqual(log_lines.realne_test_fail_type, "test")
        self.assertEqual(log_lines.realne_test_fail_returned, 1.)
        self.assertEqual(log_lines.realne_test_fail_expected, 1.)
        self.assertAlmostEqual(log_lines.realne_test_fail_diff, 0.)
        self.assertGreaterEqual(log_lines.realne_test_pass_diff, 0.)
    
    def test_log_integer_test(self):
        self.assertTrue(log_lines.integer_test_pass_present)
        self.assertEqual(log_lines.integer_test_pass_rc, 0)
        self.assertEqual(log_lines.integer_test_pass_type, "test")
        self.assertEqual(log_lines.integer_test_pass_returned, 1.)
        self.assertEqual(log_lines.integer_test_pass_expected, 1.)
        self.assertEqual(log_lines.integer_test_pass_diff, 0.)
    
    def test_log_integer_test_fail(self):
        self.assertTrue(log_lines.integer_test_fail_present)
        self.assertEqual(log_lines.integer_test_fail_rc, 1)
        self.assertEqual(log_lines.integer_test_fail_type, "test")
        self.assertEqual(log_lines.integer_test_fail_returned, 1.)
        self.assertEqual(log_lines.integer_test_fail_expected, 0.)
        self.assertNotEqual(log_lines.integer_test_fail_diff, 0.)
    
    def test_log_string_test(self):
        self.assertTrue(log_lines.string_test_present)
        self.assertEqual(log_lines.string_test_rc, 0)
        self.assertEqual(log_lines.string_test_type, "test")
        self.assertEqual(log_lines.string_test_returned, "a")
        self.assertEqual(log_lines.string_test_expected, "a")
    
    def test_log_string_test_fail(self):
        self.assertTrue(log_lines.string_test_fail_present)
        self.assertEqual(log_lines.string_test_fail_rc, 1)
        self.assertEqual(log_lines.string_test_fail_type, "test")
        self.assertEqual(log_lines.string_test_fail_returned, "a")
        self.assertEqual(log_lines.string_test_fail_expected, "b")
    
    def test_log_duration(self):
        self.assertGreater(log_lines.duration, 0.)
        self.assertLess(log_lines.duration, 120.)
        duration = datetime.fromisoformat(log_lines.last_time) - datetime.fromisoformat(log_lines.first_time)
        
        # Once, this failed for flang-7 for unknown reasons.
        self.assertAlmostEqual(log_lines.duration, duration.total_seconds(), places=1)
    
    def test_logical_failure(self):
        # This helps test whether the log file notes the correct number of failures. The number of failures could be set to zero and all the tests could pass! That's difficult to detect in the Fortran tests themselves without duplicate code.
        self.assertEqual(log_lines.logical_test_fail_failures, 1)

if __name__ == "__main__":
    #unittest.main(verbosity=2)
    unittest.main()
