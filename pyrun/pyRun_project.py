import os,sys,subprocess
import numpy as np
import matplotlib.pyplot as plt

def linAlgtest(curDir, A_file, B_file):
     path = '../code'
     #dir = "finalProj_129"
     os.chdir(path)
     #os.getcwd()
     #os.chdir(dir)
     curDir =  os.getcwd()
     #print curDir

     with open(A_file, 'r') as f:
        A = [[int(num) for num in line.split(' ')] for line in f]

     A.pop(0)
     print ("A: \n")
     print(A)

     o = open(B_file, 'r') 
     v = o.read()
     b = v.split('\n')
     b.pop(0)
     b = b[:-1]
     b_ = []
     for e in b: b_.append(int(e))
     print ("b: \n")
     print(b_)

     x = np.linalg.solve(A, b_)
     print ("solution: \n")
     print(x)


if __name__ == "__main__":

      curDir = os.getcwd()
      #print "In PyRun"
      #print curDir
      linAlgtest(curDir, "A_1.dat", "B_1.dat")
      print ("______________________________: \n")
      linAlgtest(curDir, "A_2.dat", "B_2.dat")
      print ("______________________________: \n")
      linAlgtest(curDir, "A_3.dat", "B_3.dat")
      print ("______________________________: \n")
      #linAlgtest(curDir, "A_4.dat", "B_4.dat")

