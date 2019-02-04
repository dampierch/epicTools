# -*- coding: utf-8 -*-
"""
Created on Thu May 18 02:58:42 2017

@author: CHD5N
"""

# function for screening for clinically significant hyperkalemia
def hyperkalemia_screen():
    print("1. Order an ECG") # order should be automatic
    print("2. Check for peaked T waves (tall [ie > R], narrow, symmetric)")
    print("\nOther stuff that can happen:")
    print("    (a) short QT interval")
    print("    (b) long PR duration")
    print("\nAnd eventually:")
    print("    (c) widened QRS complex")
    print("    (d) absent P waves")

# function for stabilizing cardiac membrane in case of clinically significant hyperkalemia
def stabilize_membrane():
    print("\nTo stabilize the membrane, give:")
    print("1. IV calcium gluconate 1 gm AND/OR") # order should be automatic
    print("2. IV magnesium sulfate 1 gm")

# function for promoting intra-cellular shift of potassium    
def redistribute_K():
    print("\nTo shift K into cells, give:")
    print("1. IV insulin 10U WITH IV D50 25 gm AND/OR") # order should be automatic
    print("2. Nebulized albuterol AND/OR")
    print("3. IV sodium bicarb 50 mEq")

# function for promoting elimination of potassium from body
def eliminate_K():
    print("\nTo elimate K from the body, give:")
    print("1. IV furosemide 20 mg OR")
    print("2. IV bumetanide 1 mg (for renal elimination) AND/OR")
    print("3. PO Miralax 17 gm (for enteric elimination)")

# meta-function for stabilizing cardiac membrane and redistributing and eliminating potassium
def treat_hyperkalemia():
    stabilize_membrane()
    redistribute_K()
    eliminate_K()    

# user input to give algorithm potassium level, diet status, and access; eventually should fill automatically from EMR     
K = float(input("What is potassium? " )) # fill from Results in EMR
enteral = input("What is diet status? (regular, tube feeds, NPO): ") # fill from Nutrition in EMR
access = input("What is venous access? (central vs peripheral): ") # pull from LDAs in EMR

# check EMR for K orders since last BMP 'ordered_K = x mEq'
ordered_K = 0 # orders adjusted for mEq K, evatually should fill automatically from EMR
# check EMR for K given since last BMP 'given_K = y mEq'
given_K = 0 # mEq given already, eventually should fill automatically from EMR

# first part of algorithm should supplement K toward 4 in 20 mEq increments, 60 mEq per cycle
# it should adjust for K ordered/given since last BMP
# PIV supplements can be given in 10 mEq increments
# enteral route is preferred for gut health

if int(K*100) < 400: # this test is equivalent to testing for K < 4.0 but deals better with the float problem
    delta = int(round((4.0 - K + given_K)*100))
    delta_adj = delta - ordered_K
    if delta_adj <= 60 and delta_adj % 20 == 0: # this makes sure order is in increments of 20 mEq (unless PIV)
        meq = delta_adj
    elif delta_adj <= 60 and delta_adj % 20 != 0:
        meq = delta_adj + 10
    else:
        meq = 60 # max supplement order is 60 mEq at a time

    if delta <= 60: # for delta <= 60
        print("\nPatient needs " + str(delta) + " mEq of K. Give, then recheck BMP in 24h.")
        print("Note that " + str(given_K) + " mEq given and " + str(ordered_K) + " mEq ordered, not yet given, since last BMP. Will adjust accordingly.")

        # for patients who can receive enteral supplements
        # default will be packet due to ease of use with tube, tablets will require manual revision
        if enteral == "regular" or enteral == "reg": # this will give PO supps
            if meq > 20: # this makes sure 'packet' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl orally once by giving " + str(meq/20) + " KLOR-CON packets. Recheck BMP in 24h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl orally once by giving " + str(meq/20) + " KLOR-CON packet. Recheck BMP in 24h.") # orders should be placed automatically
        elif enteral == "tube feeds" or enteral == "tube" or enteral == "tf": # this will give supps via tube
            if meq > 20: # this makes sure 'packet' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl via tube once by giving " + str(meq/20) + " KLOR-CON packets. Recheck BMP in 24h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl via tube once by giving " + str(meq/20) + " KLOR-CON packet. Recheck BMP in 24h.") # orders should be placed automatically

        # for patients who cannot receive enteral supplements
        elif access == "central" or access == "c" or access == "cvl": # this will give supps via CVL
            if meq > 20: # this makes sure 'IVPB' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl via CVL once by giving " + str(meq/20) + " doses KCl 20 mEq IVPBs. Recheck BMP in 24h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl via CVL once by giving " + str(meq/20) + " dose KCl 20 mEq IVPB. Recheck BMP in 24h.") # orders should be placed automatically
        else: # this will give supps via PIV
            meq = delta_adj
            if meq > 10: # this makes sure 'IVPB' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl via PIV once by giving " + str(meq/10) + " doses KCl 10 mEq IVPBs. Recheck BMP in 24h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl via PIV once by giving " + str(meq/10) + " dose KCl 10 mEq IVPB. Recheck BMP in 24h.") # orders should be placed automatically

    else: # for delta > 60
        print("\nPatient needs " + str(delta) + " mEq of K. Give 60 mEq, then recheck BMP in 6h.")
        print("Note that " + str(given_K) + " mEq given and " + str(ordered_K) + " mEq ordered, not yet given, since last BMP. Will adjust accordingly.")

        # for patients who can receive enteral supplements
        # default will be packet due to ease of use with tube, tablets will require manual revision
        if enteral == "regular" or enteral == "reg": # this will give PO supps
            if meq > 20: # this makes sure 'packet' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl orally once by giving " + str(meq/20) + " KLOR-CON packets. Recheck BMP in 6h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl orally once by giving " + str(meq/20) + " KLOR-CON packet. Recheck BMP in 6h.") # orders should be placed automatically
        elif enteral == "tube feeds" or enteral == "tube" or enteral == "tf": # this will give supps via tube
            if meq > 20: # this makes sure 'packet' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl via tube once by giving " + str(meq/20) + " KLOR-CON packets. Recheck BMP in 6h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl via tube once by giving " + str(meq/20) + " KLOR-CON packet. Recheck BMP in 6h.") # orders should be placed automatically

        # for patients who cannot receive enteral supplements
        elif access == "central" or access == "c" or access == "cvl": # this will give supps via CVL
            if meq > 20: # this makes sure 'IVPB' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl via CVL once by giving " + str(meq/20) + " doses KCl 20 mEq IVPBs. Recheck BMP in 6h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl via CVL once by giving " + str(meq/20) + " dose KCl 20 mEq IVPB. Recheck BMP in 6h.") # orders should be placed automatically
        else: # this will give supps via PIV
            if delta_adj <= 60:
                meq = delta_adj
            else:
                meq = 60 # max supplement order is 60 mEq at a time

            if meq > 10: # this makes sure 'IVPB' is plural when applicable
                print("\nGive " + str(meq) + " mEq KCl via PIV once by giving " + str(meq/10) + " doses KCl 10 mEq IVPBs. Recheck BMP in 6h.") # orders should be placed automatically
            else:
                print("\nGive " + str(meq) + " mEq KCl via PIV once by giving " + str(meq/10) + " dose KCl 10 mEq IVPB. Recheck BMP in 6h.") # orders should be placed automatically

# this part of code should manage high K or determine K is normal
elif K > 6.0:
    print("\nSince K is > 6, you should screen for ECG changes AND treat the hyperkalemia.")
    print("\nTo screen for ECG changes:")
    hyperkalemia_screen()
    print("\nTo treat the hyperkalemia:")
    treat_hyperkalemia()
    print("\nLastly, recheck BMP in 6 hours.")

elif K > 5.0:
    print("\nSince K is > 5, you should screen for ECG changes.")
    hyperkalemia_screen()
    screen = input("\nIs ECG normal? (Y/N) ")
    if screen == "yes" or screen == "Y" or screen == "y":
        print("\nSince ECG normal, recheck BMP in 6 hours. No intervention necessary.")
    else:
        print("\nWith ECG changes, treat for hyperkalemia:")
        treat_hyperkalemia()
        print("\nLastly, recheck BMP in 6 hours.")

else:
    print("\nK is within target range.")