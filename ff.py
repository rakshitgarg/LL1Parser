import ply.lex as lex
all=input("Enter the data")

tokens=['p','o']
literals="+*()-/"

t_p=r'\w+'
t_o=r'[0-9]+'

t_ignore=' \t\n'
lexer=lex.lex()
lexer.input(all)
inputlist=[]
for tok in lexer:
	inputlist.append(tok.type)
inputlist.append('$')
print(inputlist)

class FirstFollow:
	def __init__(self):#need to pass the grammar here..
		self.gram={'E':['TX','$'],'X':['+TX','-TX','e'],'T':['FY'],'Y':['*FY','/FY','e'],'F':['(E)','o','p']}
		self.term=['+','*','(',')','$','p','e','o','-','/']
		self.nonterm=['E','T','X','Y','F']
	
	def first(self,ip):#ip is a string; first() returns first set!!
        #print('in First')
        #length=len(ip)
		fir=[]
		ctr=0
		length=0
		if(ip in self.term):
			if ip not in fir:	
				fir.extend(ip)
		else:
			for i in self.gram[ip]:

				if(i[0] in self.term):

					if(i[0] not in fir):	 
						fir.extend(i[0])
				else:

					length=len(i)
					while(ctr<length):

						if('e' in self.gram[i[ctr]]):   # 'e' is for null symbol

							fir.extend(self.first(i[ctr]))
                                #print(fir) 
							ctr+=1
	#						print(ctr)
						else:

							fir.extend(self.first(i[ctr]))
                                #print(fir)
							break
		for i in fir:
			if i=='$':
				fir.remove(i)
		return list(set(fir))

	def follow(self,ip):
		foll=[]
		if(ip=='E'):
			foll.extend('$')
		for key in self.gram.keys():#iterating thorugh the keys of the grammar
			vals=self.gram[key]

			for each in vals:

				ctr=0
				length=len(each)

				for j in each:

					if(j==ip):

						if(ctr<length-1):

							if((ip != key)and('e' in self.first(each[ctr+1]))):

								for x in self.first(each[ctr+1]):
									if((x not in foll)and(x!='e')):
										foll.extend(x)
								for x in self.follow(key):
									if((x not in foll)and(x!='e')):
										foll.extend(x)

							else:

								for x in self.first(each[ctr+1]):
									if((x not in foll)and(x!='e')):
										foll.extend(x)
                                #print('foll',foll)
						if((ip != key)and(ctr==length-1)):

							for x in self.follow(key):
								if((x not in foll)and(x!='e')):
									foll.extend(x)
                            #print('foll',foll)
					ctr+=1
				ctr=0
		return foll

a=FirstFollow()
print()
print("First")
print('First of E : ',a.first('E'))
print('First of T : ',a.first('T'))
print('First of X : ',a.first('X'))
print('First of Y : ',a.first('Y'))
print('First of F : ',a.first('F'))


print()
print("Follow")
print('Follow of E : ',a.follow('E'))
print('Follow of T : ',a.follow('T'))
print('Follow of X : ',a.follow('X'))
print('Follow of Y : ',a.follow('Y'))
print('Follow of F : ',a.follow('F'))
print()


#Parsing table generation

table={}
for i in a.nonterm:
	table[i]={}
#print(table)
for i in table:
	for j in a.term:
		table[i][j]=[]
#print(table)		

for i in a.gram:
	for j in a.gram[i]:
		fir=[]
		fir.extend(a.first(j[0]))
		if('e' not in fir):
			for k in fir:
				table[i][k].append(str(i+'->'+j))
		else:
			fol=a.follow(i)
			#print(fir())
			fir.remove('e')
			for k in fir:
				table[i][k].append(str(i+'->'+j))
			for l in fol:
				table[i][l].append(str(i+'->'+j))
		

for i in table:
	for j in table[i]:
		if table[i][j]:
			print("Entries for "+i+"   "+j+"  are : "+str(table[i][j]))
					


class Stack:
	stack=['$']
	def __init__(self,a):
		self.a=a
		self.ele='$'
		self.stack.append(str(a))

	def push(self, x):
		self.stack.append(x)

	def pop(self):
		if(len(self.stack)==1):
			print("Stack contains only $")
			self.ele='$'
		else:
			self.ele=self.stack.pop()
		return self.ele

	def disp(self):
		#print("the elements of the stack are:")
		return(self.stack[::-1])



st=Stack('E')
print()
print("Matched       ", "Stack          ", "Input              ", "Action           ")
#inputlist=['d','+','d','*','d','$']
flag=True
flag1=True
flag2=True
matched=[]
action=[]
count=1
while(flag1):
	print()
	print(matched,"      ",st.disp(), "      ", inputlist, "         ", action) 
	i=inputlist[0]
	#print(i)
	if(i=='$'):
		#ele=st.pop()
		if(len(st.stack)==1):
		#	flag=False
			flag1=False
			break
		else:
			try:
				ele=st.pop()
				#if(len(table[ele]['$'])==1):
				#print("Enterd")
				if(table[ele]['$'][0]==str(ele+'->e')):
					action.append(table[ele]['$'][0])
				#print(action)
				#ele3=st.pop()
				#print("Hello")
					
				else:
					print("Error")
		#		flag=False
					flag1=False
					break
			except(Exception):
				print("No entry in table")
				flag1=False
				break
	else:
		#print(st.disp())		
		ele=st.pop()
		#print("ele", ele)
		if(ele == '$'):
			print("Error")
			flag1=False
			break
		if(ele in a.term):
			if(ele==i):
				matched.append(i)
				action.append("match "+i)
				#print("Matched:",matched)
				inputlist.remove(i)
				#action.append("matched"+i)
				#print("inputlist:", inputlist)
			else:
				print("terminal does not match input")
				flag1=False
				break
		else:
			#flag2=True
			#while(flag2):
			try:
				x=table[ele][i]
				#	print(x,len(x))
				#	flag2=False
			except(Exception):

				print("Mismatch symbols. No entry in table")
				flag1=False
				break
					
			if(len(x)==0):

				print("error")
				flag1=False
				break
			else:
				action.append(x[0])
				y=x[0][::-1]
				#print(y)
				for k in y:
					if(k=='e'):
						break
						
					if(k=='>'):
						break
					else:
						st.push(k)
					
	


if(len(st.stack)==1 and st.stack[0]=='$' and len(inputlist)==1 and inputlist[0]=='$'):
	
	print("Parsing successfull")
else:
	print("Error. Statement not correct") 		
			
		
