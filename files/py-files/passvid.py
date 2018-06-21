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

looking_for_left = range(10,15)
looking_for_right = range(10,15)

gs = [10,10]
coordinates = []

def PolyArea(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

def ardennes(x,y):
    a = (22,121)
    b = (114,254)
    c = (292,201)
    d = (154,76)
    p = (x,y)
    st = PolyArea((a[0],p[0],b[0]),(a[1],p[1],b[1]))+ \
    PolyArea((a[0],p[0],d[0]),(a[1],p[1],d[1]))+ \
    PolyArea((d[0],p[0],c[0]),(d[1],p[1],c[1]))+ \
    PolyArea((c[0],p[0],b[0]),(c[1],p[1],b[1]))
    if st > PolyArea((a[0],b[0],c[0],d[0]),(a[1],b[1],c[1],d[1])): 
        return False
    else: return True

def gib(x,y):
    a = (11,94)
    b = (11,234)
    c = (271,221)
    d = (271,97)
    p = (x,y)
    st = PolyArea((a[0],p[0],b[0]),(a[1],p[1],b[1]))+ \
    PolyArea((a[0],p[0],d[0]),(a[1],p[1],d[1]))+ \
    PolyArea((d[0],p[0],c[0]),(d[1],p[1],c[1]))+ \
    PolyArea((c[0],p[0],b[0]),(c[1],p[1],b[1]))
    if st > PolyArea((a[0],b[0],c[0],d[0]),(a[1],b[1],c[1],d[1])): 
        return False
    else: return True

def marie(x,y):
    a = (61,283)
    b = (195,283)
    c = (195,42)
    d = (61,42)
    p = (x,y)
    st = PolyArea((a[0],p[0],b[0]),(a[1],p[1],b[1]))+ \
    PolyArea((a[0],p[0],d[0]),(a[1],p[1],d[1]))+ \
    PolyArea((d[0],p[0],c[0]),(d[1],p[1],c[1]))+ \
    PolyArea((c[0],p[0],b[0]),(c[1],p[1],b[1]))
    if st > PolyArea((a[0],b[0],c[0],d[0]),(a[1],b[1],c[1],d[1])): 
        return False
    else: return True

def docks(x,y):
    a = (17,286)
    b = (238,296)
    c = (238,36)
    d = (103,50)
    p = (x,y)
    st = PolyArea((a[0],p[0],b[0]),(a[1],p[1],b[1]))+ \
    PolyArea((a[0],p[0],d[0]),(a[1],p[1],d[1]))+ \
    PolyArea((d[0],p[0],c[0]),(d[1],p[1],c[1]))+ \
    PolyArea((c[0],p[0],b[0]),(c[1],p[1],b[1]))
    if st > PolyArea((a[0],b[0],c[0],d[0]),(a[1],b[1],c[1],d[1])): 
        return False
    else: return True

def valk(x,y):
    a = (1,180)
    b = (70,266)
    c = (228,247)
    d = (283,80)
    e = (215,54)
    p = (x,y)
    st = PolyArea((a[0],p[0],b[0]),(a[1],p[1],b[1]))+ \
    PolyArea((a[0],p[0],e[0]),(a[1],p[1],e[1]))+ \
    PolyArea((d[0],p[0],c[0]),(d[1],p[1],c[1]))+ \
    PolyArea((d[0],p[0],e[0]),(d[1],p[1],e[1]))+ \
    PolyArea((c[0],p[0],b[0]),(c[1],p[1],b[1]))
    if st > PolyArea((a[0],b[0],c[0],d[0],e[0]),(a[1],b[1],c[1],d[1],e[1])): 
        return False
    else: return True

rectKernel = cv2.getStructuringElement(cv2.MORPH_RECT, (9, 3))
sqKernel = cv2.getStructuringElement(cv2.MORPH_RECT, (5, 5))
# fourcc = cv2.VideoWriter_fourcc(*'MJPG')
# out = cv2.VideoWriter('output.avi', fourcc, 20.0, (300,300))
team1 = 'unilad'
team2 = 'tainted'
map_name = 'valk'
cap = cv2.VideoCapture('Clips/Tainted Minds vs UNILAD _ CWL Pro League _ Stage 2 _ Week 3 Day 3_valk.mp4')
i = 1
coordinates = []
gss = []
while(True):
    looking_for_left = range(gs[0]+1,gs[0]+7)
    looking_for_right = range(gs[1]+1,gs[1]+7)
    ret, frame = cap.read()
    if ret == False:
        break
    digit1 = frame[54:94, 484:514]
    digit2 = frame[54:94, 514:544]
    digit3 = frame[54:94, 544:574]
    digit4 = frame[54:94, 703:733]
    digit5 = frame[54:94, 733:763]
    digit6 = frame[54:94, 763:793]
    if i % 15 == 0 and i/30 > 5:
        if digit1.sum() > 300000:
            d1 = predict(feature_extraction(digit1))[0]
            
        else: d1 = ""
        if digit2.sum() > 300000:
            d2 = predict(feature_extraction(digit2))[0]
            
        else: d2 = ""
        if digit3.sum() > 300000:
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

        if digit4.sum() > 300000:
            d4 = predict(feature_extraction(digit4))[0]
            
        else: d4 = ""
        if digit5.sum() > 300000:
            d5 = predict(feature_extraction(digit5))[0]
            
        else: d5 = ""
        if digit6.sum() > 300000:
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
        # if i % 30*100 == 0:
        #     print(gs)

        gs.append(round(i/30.0,1))
        gss.append(gs)
        gs = gs[0:2]
    if ret == False:
        break
    crop = frame[0:300,0:1000]
    if i%30 == 0 and i/30 > 0:
            
        # cropp = crop[45:100, 487:580]
        # cropp2 = crop[45:100, 710:794]
        # # gray = cv2.cvtColor(cropp, cv2.COLOR_BGR2GRAY)
        # # gray2 = cv2.cvtColor(cropp2, cv2.COLOR_BGR2GRAY)

        # # img2 = Image.fromarray(gray)
        # # img22 = Image.fromarray(gray2)

        # # left_score = pytesseract.image_to_string(img2, lang = 'eng')
        # # right_score = pytesseract.image_to_string(img22, lang = 'eng')


        crop_left = cv2.cvtColor(crop, cv2.COLOR_BGR2HSV)
        crop = cv2.cvtColor(crop, cv2.COLOR_BGR2HSV)
        col = crop_left[38,793]
        col2 = crop_left[38,522]

        # Team right
        lower_red2 = np.array([0,0,0])
        upper_red2 = np.array([0,0,0])
        lower_red = np.array([col[0]-11,55,100]) 
        upper_red = np.array([col[0]+11,255,255])
        if int(lower_red[0]) < 0:
            lower_red2 = np.array([180+col[0]-11,55,100])
            upper_red2 = np.array([180,255,255])
            mask =  cv2.inRange(crop_left, lower_red2, upper_red2)
            cv2.bitwise_and(crop_left,crop_left, mask= mask)
        if int(upper_red[0]) >180:
            lower_red2 = np.array([0,55,100])
            upper_red2 = np.array([180-col[0]+11,255,255])
            mask =  cv2.inRange(crop_left, lower_red2, upper_red2)
            crop_left = cv2.bitwise_and(crop_left,crop_left, mask= mask)






        # print(lower_red, upper_red, lower_red2, upper_red2)   
        mask = cv2.inRange(crop_left, lower_red, upper_red)
        res = cv2.bitwise_and(crop_left,crop, mask= mask)
        res = cv2.cvtColor(res, cv2.COLOR_BGR2GRAY)
        __,res = cv2.threshold(res, 50, 255, cv2.THRESH_BINARY)
        gradX = cv2.morphologyEx(res, cv2.MORPH_CLOSE, rectKernel)
        thresh = cv2.threshold(gradX, 0, 255,
            cv2.THRESH_BINARY | cv2.THRESH_OTSU)[1]
         
        # apply a second closing operation to the binary image, again
        # to help close gaps between credit card number regions
        res = cv2.morphologyEx(thresh, cv2.MORPH_CLOSE, sqKernel)

        res = res[0:300,0:300]
        kernel = np.ones((2,2), np.uint8)
        dilation = cv2.dilate(res, kernel, iterations=1)

        blur = cv2.GaussianBlur(dilation, (15,15), 0)

        thresh = cv2.threshold(blur,100, 255, cv2.THRESH_BINARY)[1]


        ##### Now finding Contours         ###################
        _, contours, _ = cv2.findContours(
            thresh, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

        for cnt in contours:
                # [point_x, point_y, width, height] = cv2.boundingRect(cnt)
            approx = cv2.approxPolyDP(
                cnt, 0.5 * cv2.arcLength(cnt, True), True)
            if len(approx) >= 1 and min(cv2.minAreaRect(cnt)[1]) > 0:

                if max(cv2.minAreaRect(cnt)[1])/min(cv2.minAreaRect(cnt)[1]) < 3 and \
                 min(cv2.minAreaRect(cnt)[1]) > 1 and \
                 cv2.contourArea(cnt) > 40 and cv2.contourArea(cnt) < 275:
                    M = cv2.moments(cnt)
                    cX = int(M["m10"] / M["m00"])
                    cY = int(M["m01"] / M["m00"])
                    if valk(cX,cY) == True: # can add a coordinate constraint here
                        # print(cv2.contourArea(cnt))
                        M = cv2.moments(cnt)
                        cX = int(M["m10"] / M["m00"])
                        cY = int(M["m01"] / M["m00"])
                        if [cX,cY] not in coordinates:
                            coordinates.append([team2,int(cX),int(cY), \
                                int(cv2.contourArea(cnt)), int(round(i/30.0)), team1, gs[1], map_name])
                            # rect = cv2.minAreaRect(cnt)
                            # # print(min(rect[1]))
                            # box = cv2.boxPoints(rect)
                            # box = np.int0(box)
                            # cv2.drawContours(frame,[box],0,(0,0,255),1)
        #Team left
        lower_red2 = np.array([0,0,0])
        upper_red2 = np.array([0,0,0])
        lower_red = np.array([col2[0]-12,55,100]) 
        upper_red = np.array([col2[0]+12,255,255])
        if int(lower_red[0]) < 0:
            lower_red2 = np.array([180+col2[0]-12,55,100])
            upper_red2 = np.array([180,255,255])
            mask =  cv2.inRange(crop, lower_red2, upper_red2)
            cv2.bitwise_and(crop,crop, mask= mask)
        if int(upper_red[0]) >180:
            lower_red2 = np.array([0,55,100])
            upper_red2 = np.array([180-col2[0]+12,255,255])
            mask =  cv2.inRange(crop, lower_red2, upper_red2)
            crop = cv2.bitwise_and(crop,crop, mask= mask)



        # print(lower_red, upper_red, lower_red2, upper_red2)   
        mask = cv2.inRange(crop, lower_red, upper_red)
        res = cv2.bitwise_and(crop,crop, mask= mask)
        res = cv2.cvtColor(res, cv2.COLOR_BGR2GRAY)
        __,res = cv2.threshold(res, 50, 255, cv2.THRESH_BINARY)
        gradX = cv2.morphologyEx(res, cv2.MORPH_CLOSE, rectKernel)
        thresh = cv2.threshold(gradX, 0, 255,
            cv2.THRESH_BINARY | cv2.THRESH_OTSU)[1]
         
        # apply a second closing operation to the binary image, again
        # to help close gaps between credit card number regions
        res = cv2.morphologyEx(thresh, cv2.MORPH_CLOSE, sqKernel)

        res = res[0:300,0:300]
        kernel = np.ones((2,2), np.uint8)
        dilation = cv2.dilate(res, kernel, iterations=1)

        blur = cv2.GaussianBlur(dilation, (15,15), 0)

        thresh = cv2.threshold(blur,100, 255, cv2.THRESH_BINARY)[1]


        ##### Now finding Contours         ###################
        _, contours, _ = cv2.findContours(
            thresh, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

        for cnt in contours:
                # [point_x, point_y, width, height] = cv2.boundingRect(cnt)
            approx = cv2.approxPolyDP(
                cnt, 0.5 * cv2.arcLength(cnt, True), True)
            if len(approx) >= 1 and min(cv2.minAreaRect(cnt)[1]) > 0:

                if max(cv2.minAreaRect(cnt)[1])/min(cv2.minAreaRect(cnt)[1]) < 3 and \
                 min(cv2.minAreaRect(cnt)[1]) > 1 and \
                 cv2.contourArea(cnt) > 40 and cv2.contourArea(cnt) < 275:
                    M = cv2.moments(cnt)
                    cX = int(M["m10"] / M["m00"])
                    cY = int(M["m01"] / M["m00"])
                    if valk(cX,cY) == True: # can add a coordinate constraint here
                        # print(cv2.contourArea(cnt))
                        M = cv2.moments(cnt)
                        cX = int(M["m10"] / M["m00"])
                        cY = int(M["m01"] / M["m00"])
                        if [cX,cY] not in coordinates:
                            coordinates.append([team1,int(cX),int(cY), \
                                int(cv2.contourArea(cnt)), int(round(i/30.0)), team2, gs[0], map_name])
                            # rect = cv2.minAreaRect(cnt)
                            # print(min(rect[1]))
                            # box = cv2.boxPoints(rect)
                            # box = np.int0(box)
                            # cv2.drawContours(frame,[box],0,(0,255,255),1)

        # cv2.imshow('bw', frame)

        # write the flipped frame
        # out.write(frame[0:300,0:300])
    i += 1
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    if i == -1:
        break


with open('csvs/'+team1 + team2 + map_name +".csv", "wb") as csv_file:
    writer = csv.writer(csv_file, delimiter=',')
    for line in coordinates:
        writer.writerow(line)
# with open(team1 + team2 + map_name +"score.csv", "wb") as csv_file:
#     writer = csv.writer(csv_file, delimiter=',')
#     for line in gss:
#         writer.writerow(line)
cap.release()
# out.release()
cv2.destroyAllWindows()
import os
duration = 0.5  # second
freq = 440  # Hz
os.system('play --no-show-progress --null --channels 1 synth %s sine %f' % (duration, freq))