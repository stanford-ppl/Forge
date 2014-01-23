#!/usr/bin/python

import sys, getopt, re, math

def main(argv):
	snapfile = ''
	forgefile = ''
	try:
		opts, args = getopt.getopt(argv,"h",["snap=","forge=","app=","scale="])
	except getopt.GetoptError:
		print 'test.py --snap=<path to snapfile> --forge=<path to forgefile> --app=<Betweennes or PageRank> --scale=<scaling coefficeint>'
		sys.exit(2)
	for opt, arg in opts:
		if opt in ("--snap="):
			snapfile = arg
		elif opt in ("--forge="):
			forgefile = arg
		elif opt in ("--app="):
			app = arg
		elif opt in ("--scale="):
			scale = arg
		else:
			print 'snap_output_verification.py --snap=<path to snapfile> --forge=<path to forgefile> --app=<Betweennes or PageRank> --scale=<scaling coefficeint>'
			sys.exit()
	
	fp = r'[-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?' #regex for floating point #

	hashmap = {}
	f_snap = open(snapfile,'r')
	for line in f_snap:
		#my_regex = re.compile(r'(.*)Betweenes(.*?).*')
		search_term = re.match(r'(.*)'+app+'(.*?).*', line, re.M|re.I)
		fp_match = False if re.match(r'#.*', line, re.M|re.I) else True #probably a better way
		if search_term:
			tabs_over = len(re.findall(r'\w+',search_term.group(1)))
		elif fp_match:
			line_array = re.findall(fp,line)
			hashmap[line_array[0]] = line_array[tabs_over]

	f_forge = open(forgefile,'r')
	for line in f_forge:
		fp_match = False if re.match(r'#.*', line, re.M|re.I) else True #probably a better way
		if fp_match:
			line_array = re.findall(fp,line)
			scaled_snap_number = float(hashmap[line_array[0]])*float(scale)
			if(scaled_snap_number==0 and float(line_array[1]) != 0 ):
				print "ERROR: Node " + line_array[0] + ' snap data: ' + hashmap[line_array[0]] + ' forge data: ' + line_array[1]
				sys.exit(2)
			if(scaled_snap_number != 0):
				perc_error = abs ( scaled_snap_number - float(line_array[1]) )/scaled_snap_number
				if( perc_error > 0.001 ):
					print "ERROR: Node " + line_array[0] + ' snap data: ' + hashmap[line_array[0]] + ' forge data: ' + line_array[1] + ' %ERROR: ' + str(perc_error*100)
	print "Oh snap! Success!  All data matched snap output data!"

if __name__ == "__main__":
   main(sys.argv[1:])
