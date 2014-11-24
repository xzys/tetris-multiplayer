
def search(s, sm, found):
	visit = [n for n in range(len(sm[s])) if sm[s][n] == 1 and n not in found]
	print s, visit, found
	
	for v in visit:
		found.append(s)
		search(v, sm, found)
	
	return found


if __name__ == '__main__':

	sm = [[0. for _ in range(6)] for _ in range(6)]
	
	sm[1][2] = 10.
	sm[2][3] = 10.
	sm[3][4] = 5.
	sm[2][5] = 10.
	
	sm = [[sm[j][i] if j < i else sm[i][j] for j in range(len(sm))] for i in range(len(sm))]
	print sm

	# print '\n'.join(['\t'.join([str(e) for e in row]) for row in sm])
	# print '\n'
	# print search(1, sm, [])
