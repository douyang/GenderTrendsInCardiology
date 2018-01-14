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


inputfile = open("BaselineAnalysisFileDedupWithFirstNames.csv", 'r')
outputfile = open("genderArrayBaselineDedup.csv", 'w')
outputfile2 = open("BaselineAnalysisFileDedupWithFirstNamesAndGender.csv", 'w')

listNames = []
allListNames = []

dictionaryNames = open("dictionaryNames5.csv", 'r')

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

print getGenders(["Brian","Apple","Jessica","Zaeem","NotAName","David"])

for i in range(0, len(listNames)/5):
    currentList = listNames[(i*5):(i*5+5)]
    genders = getGenders(currentList)

    print currentList, genders

    for ii in range(0,5):
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


inputfile = open("BaselineAnalysisFileDedupWithFirstNames.csv", 'r')
for line in inputfile:
    linesplit = line.strip().split(',')
    if(len(linesplit)>=14 and len(linesplit[14])>1):
        
        outputfile2.write(line.strip()+','+ str(dictNames[linesplit[14]]).strip() + "\n")
    else:
        outputfile2.write(line.strip() + "\n")

inputfile.close()
outputfile2.close()




                         
    
