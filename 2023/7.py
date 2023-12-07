from collections import Counter

with open('7input') as f:
    input = [(line.split()[0], int(line.split()[1])) for line in f]


def card_strength(c):
    if c.isdigit():
        return int(c)
    match c:
    	case 'T':
        	return 10
    	case 'J':
        	return 11
    	case 'Q':
        	return 12
    	case 'K':
        	return 13
    	case 'A':
        	return 14


def hand_type(hand):
    counter = Counter(hand)
    mc = counter.most_common()
    match mc[0][1]:
        case 5:
            return 7
        case 4:
            return 6
        case 3:
            if mc[1][1] == 2: # full house
                return 5
            return 4
        case 2:
            if mc[1][1] == 2: # two pair
                return 3
            return 2
    return 1


def score_line(hand_bid):
    hand = hand_bid[0]
    return [
        hand_type(hand),
        [card_strength(c) for c in hand]
    ]


sorted_input = sorted(input, key=lambda hand_bid: score_line(hand_bid))


print('Part 1:', sum((i+1)*bid for i, bid in enumerate(bid for hand, bid in sorted_input)))
