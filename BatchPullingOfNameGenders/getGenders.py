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


inputfile = open("NEJMallWithFirstNames.csv", 'r')
outputfile = open("genderArray3.csv", 'w')
outputfile2 = open("NEJMallWithFirstNamesAndGender.csv", 'w')

listNames = []

for line in inputfile:
    linesplit = line.strip().split(',')
    if(len(linesplit)>=10 and len(linesplit[9])>1):
        listNames.append(linesplit[9])
inputfile.close()


listNames = list(set(listNames))
#genders = getGenders(list(listNames))

dictionaryNames = open("dictionaryNames2.csv", 'r')

dictNames = ast.literal_eval(dictionaryNames.read())#{}
allGenders = []

dictionaryNames.close()

for i in range(191, len(listNames)/10):
    currentList = listNames[(i*10):(i*10+10)]
    genders = getGenders(currentList)

    print currentList, genders

    for ii in range(0,10):
        dictNames[currentList[ii]] = genders[ii]
        allGenders.append(genders[ii])

        outputfile.write(currentList[ii] + ","+ str(genders[ii]) + "\n")
        
        #time.sleep(5)
        
    

print listNames
print allGenders


dictionaryNames = open("dictionaryNames3.csv", 'w')
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
        
        outputfile.write(line.strip()+','+ dictNames[linesplit[9]].strip() + "\n")
    else:
        outputfile.write(line.strip() + "\n")

inputfile.close()
outputfile2.close()




                         
    
