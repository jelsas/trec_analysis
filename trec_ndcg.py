from math import log
from operator import div, add

def dcg( r, max=10 ):
	return reduce(lambda dcgs, dg: dcgs + [dg+dcgs[-1]], \
		map(lambda (rank, rel): (2**rel-1) / log( rank+2, 2), \
			enumerate( r[:max] ) ), [0])[1:]
def ndcg( r, max=10 ):
	return map(div, dcg(r, max), dcg(sorted(r, reverse=True), max))

if __name__ == "__main__":
	import sys
	from optparse import OptionParser
	usage = '''usage: %prog [options] qrels_file results_file
Calculates (n)DCG @ 1..10 on from a TREC-style qrels file and results file 
using the following functions:

Gain function:  2**rel - 1
Discount function: 1 / log( rank+1 )'''

	parser = OptionParser(usage=usage)
	parser.add_option('-q', '--per-query', action='store_true', \
				dest='per_query', help='print per-query output')
	parser.add_option('-d', '--dcg', action='store_true', \
				dest='no_normalize', help='don\'t normalize (DCG)')
	parser.add_option('-n', '--allow-negative', action='store_true', \
				dest='allow_neg', help='allow negative relevance values')
	(options, args) = parser.parse_args()
	(qrels_file, results_file) = args
	
	def str_to_rel(x):
		if options.allow_neg: return int(x)
		else: return max(0, int(x))
	qrels = {}
	for line in open(qrels_file):
		(qid, run, doc, rel) = line.strip().split()
		if qid not in qrels: qrels[qid] = {doc: str_to_rel(rel)}
		else: qrels[qid][doc] = str_to_rel(rel)
	
	def print_results(measure_name, qid, measure):
		for (i, m) in enumerate(measure):
			print '%s_%02d\t%s\t%f' % (measure_name, i+1, qid, m)
	
	def read_results():
		sort_scores = lambda l: [x[1] for x in sorted(l, reverse=True)]
		current_q = None
		current_scores = []
		for line in open(results_file):
			(qid, run, doc, rank, score, descr) = line.strip().split(None, 5)
			if current_q != qid:
				if current_q: yield (current_q, sort_scores(current_scores) \
												+ current_qrel.values())
				(current_q, current_qrel) = (qid, qrels.pop(qid))
				current_scores = [(float(score), current_qrel.pop(doc, 0))]
			current_scores += [(float(score), current_qrel.pop(doc, 0))]
		yield (current_q, sort_scores(current_scores) + current_qrel.values())
	
	totals = [0.0] * 10
	n_q = 0
	measure_name = (options.no_normalize and 'DCG' or 'nDCG')
	for (qid, r) in read_results():
		eval = (options.no_normalize and dcg(r) or ndcg(r))
		if options.per_query: print_results( measure_name, qid, eval )
		n_q += 1
		totals = map( add, totals, eval )
	print 'num_q\tall\t%d' % n_q
	print_results( measure_name, 'all', [n / n_q for n in totals] )