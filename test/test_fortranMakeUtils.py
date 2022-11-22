from unittest import TestCase   
import sys
import os
sys.path.append('../src')
from src.fortranMakeUtils import fortranMakeUtils as fmu


class FortranMakeUtilsTests(TestCase):


    def test_recursive(self):
        testname_dir = 'test_recursive'
        dir_fortran_files, dir_true, dir_result = self.get_directories_string(testname_dir)
        fmu.main(dir_fortran_files, 100, dir_result)
        self.assert_all_out_files(dir_true, dir_result)


    def test_ignore_routine_in_string(self):
        testname_dir = 'test_ignore_routine_in_string'
        dir_fortran_files, dir_true, dir_result = self.get_directories_string(testname_dir)
        fmu.main(dir_fortran_files, 100, dir_result)
        self.assert_all_out_files(dir_true, dir_result)


    def get_directories_string(self, testname_dir):
        dir_fortran_files = f'test/data/{testname_dir}/fortran_files'
        dir_true = f'test/data/{testname_dir}/true_files'
        dir_result = f'test/out/{testname_dir}'
        return dir_fortran_files,dir_true,dir_result


    def assert_all_out_files(self, dir_true, dir_result):
        for true_file in os.listdir(dir_true):
            true_file_path = os.path.join(dir_true, true_file)
            result_file_path = os.path.join(dir_result, true_file)
            self.assertTrue(os.path.exists(result_file_path))
            with open(true_file_path, 'r', encoding="utf-8") as true_file:
                with open(result_file_path, 'r', encoding="utf-8") as result_file:
                    self.assertEqual(true_file.read(), result_file.read())