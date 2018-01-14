import requests, json, time, ast

def getGenders(names):
	url = ""
	cnt = 0
	for name in names:
		if url == "":
			url = "name[0]=" + name
		else:
			cnt += 1
			url = url + "&name[" + str(cnt) + "]=" + name
		

	req = requests.get("http://api.genderize.io?" + url)
	results = json.loads(req.text)
	
	retrn = []
	for result in results:
		if result["gender"] is not None:
			retrn.append((result["gender"], result["probability"], result["count"]))
		else:
			retrn.append((u'None',u'0.0',0.0))
	return retrn

#if __name__ == '__main__':
	print getGenders(["Brian","Apple","Jessica","Zaeem","NotAName","David"])


inputfile = open("SecondPassAnalysisAnnotatedDedupWithFirstNames.csv", 'r')
outputfile = open("genderArrayDedup.csv", 'w')
outputfile2 = open("SecondPassAnalysisAnnotatedDedupWithFirstNamesAndGender.csv", 'w')

listNames = []
allListNames = []

dictionaryNames = open("dictionaryNames3.csv", 'r')

dictNames = ast.literal_eval(dictionaryNames.read())#{}
allGenders = []

dictionaryNames.close()

dictKeys = dictNames.keys()


for line in inputfile:
    linesplit = line.strip().split(',')
    #print len(linesplit)>=14, len(linesplit[14])>1, linesplit[14]
    
    if(len(linesplit)>=14 and len(linesplit[14])>1):
        if(linesplit[14] not in dictKeys):
                listNames.append(linesplit[14])
        allListNames.append(linesplit[14])
        
inputfile.close()


listNames = list(set(listNames))
allListNames = list(set(allListNames))

print len(listNames), len(allListNames)

#genders = getGenders(list(listNames))



for i in range(0, len(listNames)/2):
    currentList = listNames[(i*2):(i*2+2)]
    genders = getGenders(currentList)

    print currentList, genders

    for ii in range(0,2):
        dictNames[currentList[ii]] = genders[ii]
        allGenders.append(genders[ii])

        outputfile.write(currentList[ii] + ","+ str(genders[ii]) + "\n")
        
        #time.sleep(5)
        
    

print listNames
print allGenders


dictionaryNames = open("dictionaryNamesTotal.csv", 'w')
dictionaryNames.write(str(dictNames))
dictionaryNames.close()


#for i in range(0,len(listNames)):
#    outputfile.write(listNames[i], allGenders[i])
#    dictNames[listNames[i]] = genders[i]

outputfile.close()


inputfile = open("NEJMallWithFirstNames.csv", 'r')
for line in inputfile:
    linesplit = line.strip().split(',')
    if(len(linesplit)>=10 and len(linesplit[9])>1):
        
        outputfile2.write(line.strip()+','+ str(dictNames[linesplit[9]]).strip() + "\n")
    else:
        outputfile2.write(line.strip() + "\n")

inputfile.close()
outputfile2.close()




                         
    
