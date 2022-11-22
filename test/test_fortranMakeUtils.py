from unittest import TestCase   
import sys
import os
sys.path.append('../src')
from src.fortranMakeUtils import fortranMakeUtils as fmu


class FortranMakeUtilsTests(TestCase):

    def test_recursive(self):
        print('=============================================', os.getcwd())
        dir_fortran_files = 'test/data/test_recursive/fortran_files'
        dir_true = 'test/data/test_recursive/true_files'
        dir_result = 'test/out/test_recursive'
        fmu.main(dir_fortran_files, 100, dir_result)

        for true_file in os.listdir(dir_true):
            true_file_path = os.path.join(dir_true, true_file)
            result_file_path = os.path.join(dir_result, true_file)
            self.assertTrue(os.path.exists(result_file_path))
            with open(true_file_path, 'r', encoding="utf-8") as true_file:
                with open(result_file_path, 'r', encoding="utf-8") as result_file:
                    self.assertEqual(true_file.read(), result_file.read())
