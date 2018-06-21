import cv2
import csv
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image
import pytesseract
import os
import scipy.ndimage
from skimage.feature import hog
from skimage import data, color, exposure
from sklearn.model_selection import  train_test_split
from sklearn.neighbors import KNeighborsClassifier
from sklearn.externals import joblib
import scipy.ndimage

knn = joblib.load('/home/dougliebe/Documents/CoD/knn_model.pkl')
def feature_extraction(image):
    return hog(color.rgb2gray(image), orientations=8, pixels_per_cell=(5, 9),\
     cells_per_block=(8, 3))
def predict(df):
    predict = knn.predict(df.reshape(1,-1))[0]
    predict_proba = knn.predict_proba(df.reshape(1,-1))
    return predict, predict_proba[0][predict]

looking_for_left = range(0,5)
looking_for_right = range(0,5)

gs = [0,0]
cap = cv2.VideoCapture('Clips/Tainted Minds vs OpTic Gaming _ CWL Pro League _ Stage 2 _ Week 3 Day 2_marie.mp4')
coordinates = []
i = 0
while(True):
    looking_for_left = range(gs[0]+1,gs[0]+7)
    looking_for_right = range(gs[1]+1,gs[1]+7)
    _, frame = cap.read()
    if _ == False:
        break
    digit1 = frame[54:94, 484:514]
    digit2 = frame[54:94, 514:544]
    digit3 = frame[54:94, 544:574]
    digit4 = frame[54:94, 703:733]
    digit5 = frame[54:94, 733:763]
    digit6 = frame[54:94, 763:793]
    if i % 15 == 0 and i/30 > 5:
        if digit1.sum() > 250000:
            d1 = predict(feature_extraction(digit1))[0]
            
        else: d1 = ""
        if digit2.sum() > 250000:
            d2 = predict(feature_extraction(digit2))[0]
            
        else: d2 = ""
        if digit3.sum() > 250000:
            d3 = predict(feature_extraction(digit3))[0]
        else: d3 = ""
        val = str(d1)+str(d2)+str(d3)
        if val == "": val = '0'
        for num in looking_for_left:
            if str(num) == val[0:len(str(num))]:
                gs[0] = num

        # if left_score[0] != prev_left_score[0] and \
        # left_score[1] != prev_left_score[1] and \
        # left_score[2] != prev_left_score[2]:
        #         gs[0] += 1

        if digit4.sum() > 250000:
            d4 = predict(feature_extraction(digit4))[0]
            
        else: d4 = ""
        if digit5.sum() > 250000:
            d5 = predict(feature_extraction(digit5))[0]
            
        else: d5 = ""
        if digit6.sum() > 250000:
            d6 = predict(feature_extraction(digit6))[0]
        else: d6 = ""
        val = str(d4)+str(d5)+str(d6)
        if val == "": val = '0'
        for num in looking_for_right:
            if str(num) == val[0:len(str(num))]:
                gs[1] = num
        # if right_score[0] != prev_right_score[0] and \
        # right_score[1] != prev_right_score[1] and \
        # right_score[2] != prev_right_score[2]:
        #         gs[0] += 1
        # if right_score[0] != prev_right_score[0] and \
        # right_score[1] != prev_right_score[1] and \
        # right_score[2] == prev_right_score[2]:
        #         gs[0] += 1
        if i % 30*10 == 0:
            print(gs)

        gs.append(round(i/30.0,1))
        coordinates.append(gs)
        gs = gs[0:2]
    cv2.imshow('frame',frame)
    # print(left_score)
    i += 1
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    if i == -1:
        break

# print(coordinates)
# a = np.asarray(score)
with open("taintopticmariescore.csv", "wb") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        for line in coordinates:
            writer.writerow(line)

# np.savetxt("digit3.csv", score, delimiter=",")
cap.release()
cv2.destroyAllWindows()