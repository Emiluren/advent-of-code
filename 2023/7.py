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


def card_strength2(c):
    if c == 'J':
        return 1
    if c.isdigit():
        return int(c)
    match c:
    	case 'T':
        	return 10
    	case 'Q':
        	return 12
    	case 'K':
        	return 13
    	case 'A':
        	return 14


def hand_type2(hand):
    c = Counter(hand)
    mc = c.most_common()

    max0 = mc['J']
    for card, count in mc:
        if card != 'J':
            max0 += count
            break

    if max0 == 5:
        return 7
    elif max0 == 4:
        return 6

    # TODO: Not finished from here down
    def score3_card():
        if mc[1][1] == 2: # full house
            return 5
        return 4

    match mc[0][1]:
        case 3:
            return score3_card()
        case 2:
            if mc[1][1] == 2: # two pair
                return 3
            return 2
    return 1


def score_line2(hand_bid):
    hand = hand_bid[0]
    return [
        hand_type2(hand),
        [card_strength2(c) for c in hand]
    ]


sorted_input2 = sorted(input, key=lambda hand_bid: score_line2(hand_bid))
print('Part 2:', sum((i+1)*bid for i, bid in enumerate(bid for hand, bid in sorted_input2)))
