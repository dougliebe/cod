import cv2
import numpy as np
import matplotlib.pyplot as plt
rectKernel = cv2.getStructuringElement(cv2.MORPH_RECT, (9, 3))
sqKernel = cv2.getStructuringElement(cv2.MORPH_RECT, (5, 5))

img = cv2.imread('greentest.png', 1)
# can use 0 = cv2.IMREAD_GRAYSCALE
# 1 = COLOR
# -1 = UNCHANGED
img = img[0:300, 0:1000]
t1col = img[38,793]
t2col = img[38,522]
crop = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
col = crop[38,793]
col2 = crop[38,522]
print(col2)

# # Team right
# lower_red2 = np.array([0,0,0])
# upper_red2 = np.array([0,0,0])
# lower_red = np.array([col[0]-11,75,100]) 
# upper_red = np.array([col[0]+11,255,255])
# if int(lower_red[0]) < 0:
#     lower_red2 = np.array([180+col[0]-11,75,100])
#     upper_red2 = np.array([180,255,255])
#     mask =  cv2.inRange(crop, lower_red2, upper_red2)
#    # crop = cv2.bitwise_and(crop,crop, mask= mask)
# if int(upper_red[0]) >180:
#     lower_red2 = np.array([0,75,100])
#     upper_red2 = np.array([180-col[0]+11,255,255])
#     mask =  cv2.inRange(crop, lower_red2, upper_red2)
#     # crop = cv2.bitwise_and(crop,crop, mask= mask)

#Team left
lower_red2 = np.array([0,0,0])
upper_red2 = np.array([0,0,0])
lower_red = np.array([col2[0]-12,50,80]) 
upper_red = np.array([col2[0]+12,255,255])
if int(lower_red[0]) < 0:
    lower_red2 = np.array([180+col2[0]-12,50,80])
    upper_red2 = np.array([180,255,255])
    mask =  cv2.inRange(crop, lower_red2, upper_red2)
    # crop = cv2.bitwise_and(crop,crop, mask= mask)
if int(upper_red[0]) >180:
    lower_red2 = np.array([0,50,80])
    upper_red2 = np.array([180-col2[0]+12,255,255])
    mask =  cv2.inRange(crop, lower_red2, upper_red2)
    # crop = cv2.bitwise_and(crop,crop, mask= mask)


# print(mask)
print(lower_red, upper_red, lower_red2, upper_red2)
if 'mask' in globals():  
    mask += cv2.inRange(crop, lower_red, upper_red)
else: mask = cv2.inRange(crop, lower_red, upper_red)
res = cv2.bitwise_and(crop,crop, mask= mask)
res = cv2.cvtColor(res, cv2.COLOR_BGR2GRAY)
__,res = cv2.threshold(res, 50, 255, cv2.THRESH_BINARY)
gradX = cv2.morphologyEx(res, cv2.MORPH_CLOSE, rectKernel)
thresh = cv2.threshold(gradX, 0, 255,
    cv2.THRESH_BINARY | cv2.THRESH_OTSU)[1]
thresh2 = cv2.threshold(gradX, 0, 255,
    cv2.THRESH_BINARY | cv2.THRESH_OTSU)[1]
 
# apply a second closing operation to the binary image, again
# to help close gaps between credit card number regions
res = cv2.morphologyEx(thresh, cv2.MORPH_CLOSE, sqKernel)

res = res[0:300,0:300]
kernel = np.ones((2,2), np.uint8)
dilation = cv2.dilate(res, kernel, iterations=1)

blur = dilation#cv2.GaussianBlur(dilation, (9,9), 0)


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
coordinates = []
for cnt in contours:
        # [point_x, point_y, width, height] = cv2.boundingRect(cnt)
    approx = cv2.approxPolyDP(
        cnt, 0.5 * cv2.arcLength(cnt, True), True)
    if len(approx) >= 1 and min(cv2.minAreaRect(cnt)[1]) > 0:

    	if max(cv2.minAreaRect(cnt)[1])/min(cv2.minAreaRect(cnt)[1]) < 2.7 and \
         min(cv2.minAreaRect(cnt)[1]) > 1 and \
         cv2.contourArea(cnt) > 40 and cv2.contourArea(cnt) < 1000:
            M = cv2.moments(cnt)
            cX = int(M["m10"] / M["m00"])
            cY = int(M["m01"] / M["m00"])
            if cX in range(64,200) and cY in range(36,282):
                print(cv2.contourArea(cnt))
                M = cv2.moments(cnt)
                cX = int(M["m10"] / M["m00"])
                cY = int(M["m01"] / M["m00"])
                if [cX,cY] not in coordinates:
                    coordinates.append([cX,cY])
                    rect = cv2.minAreaRect(cnt)
                    print(min(rect[1]))
                    box = cv2.boxPoints(rect)
                    box = np.int0(box)
                    cv2.drawContours(img,[box],0, (51,255,51),1)
                    # cv2.rectangle(img, (int(cX)-10, int(cY)+10),(int(cX)+10, int(cY)-10), (int(t2col[0]), int(t2col[1]), int(t2col[2])), 2)

print(coordinates)
cv2.imshow("result", img[0:300,0:300])
# new = cv2.threshold(res, 50, 255, cv2.THRESH_BINARY)
# crop = cv2.cvtColor(crop, cv2.COLOR_BGR2HSV)
# retval, crop = cv2.threshold(crop, 100, 150, cv2.THRESH_BINARY)
# cv2.imshow('Adaptive threshold',th)
cv2.imshow('image',hsv)
# cv2.imwrite('ex.png', thresh[92:(92+20),151:(151+40)])
cv2.imshow('orig',thresh)
cv2.waitKey(0)
cv2.destroyAllWindows()

