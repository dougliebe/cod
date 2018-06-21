import numpy as np
import cv2
from PIL import Image
import pytesseract
import matplotlib.pyplot as plt
import csv

def PolyArea(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

def ardennes(x,y):
    a = (13,132)
    b = (122,269)
    c = (274,207)
    d = (160,54)
    p = (x,y)
    st = PolyArea((a[0],p[0],b[0]),(a[1],p[1],b[1]))+ \
    PolyArea((a[0],p[0],d[0]),(a[1],p[1],d[1]))+ \
    PolyArea((d[0],p[0],c[0]),(d[1],p[1],c[1]))+ \
    PolyArea((c[0],p[0],b[0]),(c[1],p[1],b[1]))
    if st > PolyArea((a[0],b[0],c[0],d[0]),(a[1],b[1],c[1],d[1])): 
        return False
    else: return True

rectKernel = cv2.getStructuringElement(cv2.MORPH_RECT, (9, 3))
sqKernel = cv2.getStructuringElement(cv2.MORPH_RECT, (5, 5))
# fourcc = cv2.VideoWriter_fourcc(*'MJPG')
# out = cv2.VideoWriter('output.avi', fourcc, 20.0, (300,300))

cap = cv2.VideoCapture('Clips/OpTic Gaming vs UNILAD _ CWL Pro League _ Stage 2 _ Week 3 Day 1_docks.mp4')
i = 1

coordinates = []
while(True):
    ret, frame = cap.read()

    if ret == False:
        break
    crop = frame[0:300,0:1000]
    if i%15 == 0 and i/30 > 0:
            
        # cropp = crop[45:100, 487:580]
        # cropp2 = crop[45:100, 710:794]
        # # gray = cv2.cvtColor(cropp, cv2.COLOR_BGR2GRAY)
        # # gray2 = cv2.cvtColor(cropp2, cv2.COLOR_BGR2GRAY)

        # # img2 = Image.fromarray(gray)
        # # img22 = Image.fromarray(gray2)

        # # left_score = pytesseract.image_to_string(img2, lang = 'eng')
        # # right_score = pytesseract.image_to_string(img22, lang = 'eng')

        t1col = crop[38,793]
        t2col = crop[38,522]
        crop = cv2.cvtColor(crop, cv2.COLOR_BGR2HSV)
        col = crop[38,793]
        col2 = crop[38,522]

        # # Team right
        # lower_red2 = np.array([0,0,0])
        # upper_red2 = np.array([0,0,0])
        # lower_red = np.array([col[0]-11,75,100]) 
        # upper_red = np.array([col[0]+11,255,255])
        # if int(lower_red[0]) < 0:
        #     lower_red2 = np.array([180+col[0]-11,75,100])
        #     upper_red2 = np.array([180,255,255])
        #     mask =  cv2.inRange(crop, lower_red2, upper_red2)
        #     cv2.bitwise_and(crop,crop, mask= mask)
        # if int(upper_red[0]) >180:
        #     lower_red2 = np.array([0,75,100])
        #     upper_red2 = np.array([180-col[0]+11,255,255])
        #     mask =  cv2.inRange(crop, lower_red2, upper_red2)
        #     crop = cv2.bitwise_and(crop,crop, mask= mask)



        #Team left
        lower_red2 = np.array([0,0,0])
        upper_red2 = np.array([0,0,0])
        lower_red = np.array([col2[0]-12,50,85]) 
        upper_red = np.array([col2[0]+12,255,255])
        if int(lower_red[0]) < 0:
            lower_red2 = np.array([180+col2[0]-12,50,85])
            upper_red2 = np.array([180,255,255])
            mask =  cv2.inRange(crop, lower_red2, upper_red2)
            cv2.bitwise_and(crop,crop, mask= mask)
        if int(upper_red[0]) >180:
            lower_red2 = np.array([0,50,85])
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

        blur = dilation#cv2.GaussianBlur(dilation, (5,5), 0)
        # mask = np.zeros(blur.shape, dtype=np.uint8)
        # roi_corners = np.array([[(13,132), (122,269), (274,207), (160,54)]], dtype=np.int32)
        # # fill the ROI so it doesn't get wiped out when the mask is applied
        # channel_count = -1#blur.shape[3]  # i.e. 3 or 4 depending on your image
        # ignore_mask_color = (255,)*channel_count
        # blur = cv2.fillConvexPoly(mask, roi_corners, ignore_mask_color)

        thresh = cv2.threshold(blur,100, 255, cv2.THRESH_BINARY)[1]

        # template = cv2.imread('ex.png',0)
        # # template2 = cv2.imread('2.png',0)
        # # template3 = cv2.imread('3.png',0)
        # # template4 = cv2.imread('4.png',0)
        # w, h = template.shape[::-1]

        # res = cv2.matchTemplate(thresh,template,cv2.TM_CCOEFF_NORMED)
        # # res2 = cv2.matchTemplate(thresh,template2,cv2.TM_CCOEFF_NORMED)
        # # res3 = cv2.matchTemplate(thresh,template3,cv2.TM_CCOEFF_NORMED)
        # # res4 = cv2.matchTemplate(thresh,template4,cv2.TM_CCOEFF_NORMED)

        # threshold = 0.65

        # loc = np.where( res >= threshold)# or res2 >= threshold or res3 >= threshold or res4 >= threshold)
        # # loc2 = np.where( res2 >= threshold)
        # # loc3 = np.where( res3 >= threshold)
        # # loc4 = np.where( res4 >= threshold)

        # coordinates = []
        # for pt in zip(*loc[::-1]):
        #     if pt[0] in range(64,200) and pt[1] in range(36,300):
        #         coordinates.append([pt[0],pt[1]])
        #         cv2.rectangle(img, pt, (pt[0] + w/2, pt[1] + h), (0,0,255), 1)
        # for pt in zip(*loc2[::-1]):
        #     if pt[0] in range(64,200) and pt[1] in range(36,300):
        #         coordinates.append([pt[0],pt[1]])
        #         cv2.rectangle(img, pt, (pt[0] + w, pt[1] + h), (0,0,255), 2)
        # for pt in zip(*loc3[::-1]):
        #     if pt[0] in range(64,200) and pt[1] in range(36,300):
        #         coordinates.append([pt[0],pt[1]])
        #         cv2.rectangle(img, pt, (pt[0] + w, pt[1] + h), (0,0,255), 2)
        # for pt in zip(*loc4[::-1]):
        #     if pt[0] in range(64,200) and pt[1] in range(36,300):
        #         coordinates.append([pt[0],pt[1]])
        #         cv2.rectangle(img, pt, (pt[0] + w, pt[1] + h), (0,0,255), 2)


        # font                   = cv2.FONT_HERSHEY_SIMPLEX
        # fontScale              = 0.5
        # fontColor              = (255,255,255)
        # lineType               = 2

        ##### Now finding Contours         ###################
        _, contours, _ = cv2.findContours(
            thresh, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

        for cnt in contours:
                # [point_x, point_y, width, height] = cv2.boundingRect(cnt)
            approx = cv2.approxPolyDP(
                cnt, 0.1 * cv2.arcLength(cnt, True), True)
            if len(approx) >= 1 and min(cv2.minAreaRect(cnt)[1]) > 0:

                if max(cv2.minAreaRect(cnt)[1])/min(cv2.minAreaRect(cnt)[1]) < 3 and \
                 min(cv2.minAreaRect(cnt)[1]) > 1 and \
                 cv2.contourArea(cnt) > 40 and cv2.contourArea(cnt) < 300:
                    M = cv2.moments(cnt)
                    cX = int(M["m10"] / M["m00"])
                    cY = int(M["m01"] / M["m00"])
                    if ardennes(cX,cY) == True: # can add a coordinate constraint here
                        # print(cv2.contourArea(cnt))
                        M = cv2.moments(cnt)
                        cX = int(M["m10"] / M["m00"])
                        cY = int(M["m01"] / M["m00"])
                        if [cX,cY] not in coordinates:
                            coordinates.append([int(cX),int(cY), \
                                int(cv2.contourArea(cnt)), int(round(i/30.0))])
                            rect = cv2.minAreaRect(cnt)
                            # print(min(rect[1]))
                            box = cv2.boxPoints(rect)
                            box = np.int0(box)
                            cv2.drawContours(frame,[box],0,(0,255,255),1)
            # cv2.imshow('frame',thresh)

        cv2.imshow('bw', frame)

        # write the flipped frame
        # out.write(frame[0:300,0:300])
    i += 1
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    if i == -1:
        break

# print(coordinates)
# a = np.asarray(coordinates)
# np.savetxt("coluni.csv", a, delimiter=",")
# with open("coluni_left.csv", "wb") as csv_file:
#         writer = csv.writer(csv_file, delimiter=',')
#         for line in coordinates:
#             writer.writerow(line)
cap.release()
# out.release()
cv2.destroyAllWindows()