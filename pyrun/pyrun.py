# pyrun.py
# Final Project for AM129 , Fall 2019
# Written by Ashley Pauley and Kevin Lo


import os, sys, subprocess
import numpy as np
import matplotlib.pyplot as plt


def make_make():
	dir = "../code/"
	path = dir + "main.exe"
	cwd = os.getcwd()
	os.chdir(dir)
	if os.path.exists(path):
		##print("file did exist..")
		os.system("make clean")
	os.system("make")
	os.chdir(cwd)
      
def run_linear_solve():
    dir = "../code/"
    path = dir + "main.exe"
    cwd = os.getcwd()
    os.chdir(dir)
    if os.path.exists(path):
        os.system("./main.exe")
    else:
        print("Error:  main.exe did not exist\n")
        return
    os.chdir(cwd)

    
if __name__ == '__main__':
	print(" IN MAIN..\n")
	make_make()
	print(" RUN MAKE..\n")
	run_linear_solve()
	#A_

