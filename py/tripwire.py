#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

# Run tests with `python3 -m unittest tripwire.py`.

import unittest

TRIPWIRE_DIRECTIVE_PREFIX = "tripwire$"
TRIPWIRE_BEGIN_DIRECTIVE  = TRIPWIRE_DIRECTIVE_PREFIX+" begin"

def crc32(x):
    from zlib import crc32
    
    checksum_integer = crc32(bytes(x, "utf-8"))
    
    # <https://stackoverflow.com/a/12638477/1124489>
    checksum = f"{checksum_integer:0{8}x}"
    assert len(checksum) == 8
    
    return checksum.upper()

def get_tripwire(file, checksum)
    with open(file, "r") as file_in:
        for line in file_in:
            if TRIPWIRE_BEGIN_DIRECTIVE in line:
                

if __name__ == "__main__":
    print("blah")

class Tests(unittest.TestCase):
    # <https://docs.python.org/3/library/unittest.html>
    
    def test_crc32(self):
        # <https://asecuritysite.com/hash/crc32>
        self.assertEqual(crc32("The quick brown fox jumps over the lazy dog"), "414FA339")
        self.assertEqual(crc32("Test vector from febooti.com"), "0C877F61")
        self.assertEqual(crc32(""), "00000000")
        
        # <https://lxp32.github.io/docs/a-simple-example-crc32-calculation/>
        self.assertEqual(crc32("123456789"), "CBF43926")
        
