#!/usr/bin/python

import sys, getopt, re, math

def main(argv):
	snapfile = ''
	forgefile = ''
	try:
		opts, args = getopt.getopt(argv,"h",["snap=","forge="])
	except getopt.GetoptError:
		print 'test.py --snap=<path to snapfile> --forge=<path to forgefile>'
		sys.exit(2)
	for opt, arg in opts:
		if opt in ("--snap="):
			snapfile = arg
		elif opt in ("--forge="):
			forgefile = arg
		else:
			print 'snap_output_verification.py --snap=<path to snapfile> --forge=<path to forgefile>'
			sys.exit()
	
	fp = r'[-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?' #regex for floating point #

	hashmap = {}
	f_snap = open(snapfile,'r')
	for line in f_snap:
		search_term = re.match(r'(.*)Betweennes(.*?).*', line, re.M|re.I)
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
			if( abs ( (float(hashmap[line_array[0]])*2) - float(line_array[1]) ) > 0.001 ):
				print "ERROR: Node " + line_array[0] + ' snap data: ' + hashmap[line_array[0]] + ' forge data: ' + line_array[1]
				sys.exit(2)
	print "Success!  All data matched snap output data!"

if __name__ == "__main__":
   main(sys.argv[1:])