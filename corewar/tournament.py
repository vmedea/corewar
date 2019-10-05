#! /usr/bin/env python
# coding: utf-8

from mars import *
from glob import glob
from itertools import chain

def run_tournament(warriors, environment):
    big_score_table = {}
    for warrior in warriors:
        big_score_table[warrior] = {}
    while len(warriors) > 1:
        warrior = warriors.pop()
        # could fight self, special, here
        for opponent in warriors:
            run_match([warrior, opponent], args)
        big_score_table[warrior][opponent] = (warrior.wins, warrior.ties, warrior.losses)
        big_score_table[opponent][warrior] = (opponent.wins, opponent.ties, opponent.losses)
    return big_score_table

if __name__ == "__main__":
    import argparse
    import redcode
    
    parser = argparse.ArgumentParser(description='MARS (Memory Array Redcode Simulator)')
    parser.add_argument('--rounds', '-r', metavar='ROUNDS', type=int, nargs='?',
                        default=25, help='Rounds to play')
    parser.add_argument('--size', '-s', metavar='CORESIZE', type=int, nargs='?',
                        default=8000, help='The core size')
    parser.add_argument('--cycles', '-c', metavar='CYCLES', type=int, nargs='?',
                        default=80000, help='Cycles until tie')
    parser.add_argument('--processes', '-p', metavar='MAXPROCESSES', type=int, nargs='?',
                        default=8000, help='Max processes')
    parser.add_argument('--length', '-l', metavar='MAXLENGTH', type=int, nargs='?',
                        default=100, help='Max warrior length')
    parser.add_argument('--distance', '-d', metavar='MINDISTANCE', type=int, nargs='?',
                        default=100, help='Minimum warrior distance')
    parser.add_argument('sources', metavar='SOURCES', type=glob, nargs='+',
                        help='Warrior redcode file paths')

    args = parser.parse_args()

    warrior_files = [open(sourcefile, 'r') for sourcefile in chain.from_iterable(args.sources)]
    # build environment
    environment = {'CORESIZE': args.size,
                   'CYCLES': args.cycles,
                   'ROUNDS': args.rounds,
                   'MAXPROCESSES': args.processes,
                   'MAXLENGTH': args.length,
                   'MINDISTANCE': args.distance}
    
    # assemble warriors
    warriors = []
    for file in warrior_files:
        try:
            print('loading ' + file.name)
            warriors.append(redcode.parse(file, environment))
        except ValueError as err:
            print(err)
            print('Skipping file ' + file.name)
    
    print(run_tournament(warriors, environment))