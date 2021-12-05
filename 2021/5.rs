fn main() {
    let x_min = VENTS.iter().map(|((x1, _), (x2, _))| x1.min(x2)).min().unwrap();
    let x_max = VENTS.iter().map(|((x1, _), (x2, _))| x1.max(x2)).max().unwrap();
    let y_min = VENTS.iter().map(|((_, y1), (_, y2))| y1.min(y2)).min().unwrap();
    let y_max = VENTS.iter().map(|((_, y1), (_, y2))| y1.max(y2)).max().unwrap();

    println!("x-range {} - {}", x_min, x_max);
    println!("y-range {} - {}", y_min, y_max);

    let mut crossings1 = vec![[0; 1000]; 1000];
    let mut crossings2 = vec![[0; 1000]; 1000];

    for ((x1, y1), (x2, y2)) in VENTS {
        if x1 == x2 {
            for y in (y1.min(y2))..=(y1.max(y2)) {
                crossings1[y][x1] += 1;
                crossings2[y][x1] += 1;
            }
        } else if y1 == y2 {
            for x in (x1.min(x2))..=(x1.max(x2)) {
                crossings1[y1][x] += 1;
                crossings2[y1][x] += 1;
            }
        } else {
            let mut x = x1 as i32;
            let mut y = y1 as i32;
            let dx = (x2 as i32 - x).signum();
            let dy = (y2 as i32 - y).signum();

            while (x, y) != (x2 as i32, y2 as i32) {
                crossings2[y as usize][x as usize] += 1;
                x += dx;
                y += dy;
            }
            crossings2[y2][x2] += 1;
        }
    }

    //print_map(crossings1.iter().take(10).map(|r| r.iter().take(10).cloned().collect()).collect());

    println!("Part 1: {}", count_double(&crossings1));
    println!("Part 2: {}", count_double(&crossings2));
}

fn count_double(crossings: &[[usize; 1000]]) -> usize {
    crossings.iter().map(|r| r.iter().map(|v| if *v >= 2 {
        1
    } else {
        0
    }).sum::<usize>()).sum()
}

fn print_map(m: Vec<Vec<usize>>) {
    for row in m {
        for v in row {
            if v == 0 {
                print!(".");
            } else if v < 10 {
                print!("{}", v);
            } else {
                print!("x");
            }
        }
        println!();
    }
}

// const VENTS: [((usize, usize), (usize, usize)); 10] = [
//     ((0,9), (5,9)),
//     ((8,0), (0,8)),
//     ((9,4), (3,4)),
//     ((2,2), (2,1)),
//     ((7,0), (7,4)),
//     ((6,4), (2,0)),
//     ((0,9), (2,9)),
//     ((3,4), (1,4)),
//     ((0,0), (8,8)),
//     ((5,5), (8,2)),
// ];
const VENTS: [((usize, usize), (usize, usize)); 500] = [
    ((964,133), (596,133)),
    ((920,215), (920,976)),
    ((123,528), (123,661)),
    ((613,13), (407,13)),
    ((373,876), (424,876)),
    ((616,326), (120,326)),
    ((486,335), (539,388)),
    ((104,947), (54,947)),
    ((319,241), (282,204)),
    ((453,175), (453,438)),
    ((485,187), (915,617)),
    ((863,605), (603,605)),
    ((870,524), (342,524)),
    ((967,395), (634,62)),
    ((405,181), (807,181)),
    ((961,363), (419,905)),
    ((89,586), (214,461)),
    ((545,481), (731,295)),
    ((407,678), (626,678)),
    ((421,642), (91,312)),
    ((11,22), (935,946)),
    ((770,208), (76,902)),
    ((668,858), (668,890)),
    ((568,451), (574,451)),
    ((233,56), (371,56)),
    ((233,932), (44,932)),
    ((404,81), (796,81)),
    ((520,77), (403,194)),
    ((296,736), (447,887)),
    ((210,909), (16,909)),
    ((692,483), (877,668)),
    ((777,289), (744,289)),
    ((22,760), (652,130)),
    ((96,360), (626,360)),
    ((101,267), (101,783)),
    ((47,667), (660,667)),
    ((805,682), (563,440)),
    ((112,15), (463,366)),
    ((406,808), (430,808)),
    ((793,767), (107,81)),
    ((560,534), (958,534)),
    ((722,429), (722,459)),
    ((646,889), (646,195)),
    ((433,942), (449,958)),
    ((716,503), (716,99)),
    ((266,450), (266,780)),
    ((316,81), (565,81)),
    ((760,452), (687,452)),
    ((976,983), (15,22)),
    ((499,564), (499,909)),
    ((839,913), (38,112)),
    ((707,333), (438,333)),
    ((47,644), (352,644)),
    ((807,309), (807,706)),
    ((434,686), (812,308)),
    ((559,572), (63,76)),
    ((493,352), (581,352)),
    ((94,88), (928,88)),
    ((898,738), (106,738)),
    ((201,10), (564,10)),
    ((976,914), (976,472)),
    ((836,153), (585,153)),
    ((178,43), (17,204)),
    ((784,967), (738,967)),
    ((370,359), (449,359)),
    ((13,526), (637,526)),
    ((399,158), (10,158)),
    ((572,293), (289,293)),
    ((627,674), (895,674)),
    ((921,402), (984,402)),
    ((907,667), (944,704)),
    ((574,877), (882,569)),
    ((977,977), (121,121)),
    ((550,584), (862,584)),
    ((396,556), (396,289)),
    ((391,33), (532,174)),
    ((12,988), (989,11)),
    ((48,787), (48,637)),
    ((476,638), (113,638)),
    ((985,985), (13,13)),
    ((838,784), (198,784)),
    ((567,195), (677,305)),
    ((174,251), (577,654)),
    ((296,801), (53,558)),
    ((983,899), (983,380)),
    ((507,230), (507,929)),
    ((264,516), (668,920)),
    ((865,952), (865,768)),
    ((522,290), (744,512)),
    ((936,958), (936,115)),
    ((527,871), (527,519)),
    ((944,972), (21,49)),
    ((880,380), (695,565)),
    ((471,374), (446,349)),
    ((503,597), (127,221)),
    ((471,514), (30,73)),
    ((890,232), (890,511)),
    ((14,461), (14,853)),
    ((167,676), (148,676)),
    ((987,230), (754,230)),
    ((797,725), (797,847)),
    ((347,21), (84,21)),
    ((839,274), (964,274)),
    ((607,456), (894,456)),
    ((335,949), (301,949)),
    ((167,236), (820,889)),
    ((87,558), (87,917)),
    ((318,788), (622,484)),
    ((699,583), (699,321)),
    ((971,967), (35,31)),
    ((420,44), (420,36)),
    ((29,484), (458,484)),
    ((768,157), (768,30)),
    ((690,839), (317,839)),
    ((870,578), (560,578)),
    ((697,195), (70,822)),
    ((689,45), (689,223)),
    ((790,724), (341,724)),
    ((694,291), (694,507)),
    ((43,339), (43,987)),
    ((590,733), (590,179)),
    ((751,361), (945,361)),
    ((99,820), (450,469)),
    ((460,696), (942,696)),
    ((783,940), (487,644)),
    ((630,537), (48,537)),
    ((643,856), (643,396)),
    ((558,733), (257,432)),
    ((16,972), (570,418)),
    ((636,188), (636,610)),
    ((868,138), (868,407)),
    ((85,424), (85,919)),
    ((710,932), (354,576)),
    ((356,505), (783,505)),
    ((606,876), (606,62)),
    ((577,431), (749,431)),
    ((108,262), (108,145)),
    ((615,455), (264,104)),
    ((205,754), (866,754)),
    ((189,182), (855,848)),
    ((10,43), (925,958)),
    ((293,773), (293,534)),
    ((746,313), (802,369)),
    ((607,174), (211,570)),
    ((860,840), (260,240)),
    ((879,78), (595,78)),
    ((11,143), (449,143)),
    ((190,983), (267,983)),
    ((912,92), (76,928)),
    ((744,364), (744,258)),
    ((436,417), (46,807)),
    ((629,592), (517,592)),
    ((113,893), (113,959)),
    ((714,213), (786,285)),
    ((868,165), (868,731)),
    ((349,69), (491,69)),
    ((278,430), (111,263)),
    ((593,849), (593,203)),
    ((156,860), (876,860)),
    ((169,615), (169,984)),
    ((983,93), (139,937)),
    ((94,548), (18,548)),
    ((623,72), (106,589)),
    ((530,334), (473,334)),
    ((384,746), (925,205)),
    ((711,74), (28,757)),
    ((850,728), (629,949)),
    ((378,801), (228,651)),
    ((347,968), (201,822)),
    ((82,578), (82,555)),
    ((149,405), (707,963)),
    ((254,169), (793,169)),
    ((443,454), (331,454)),
    ((460,659), (608,807)),
    ((838,807), (31,807)),
    ((561,952), (290,952)),
    ((755,626), (204,75)),
    ((550,424), (550,81)),
    ((772,115), (772,600)),
    ((40,517), (40,232)),
    ((277,841), (317,841)),
    ((899,150), (128,921)),
    ((735,332), (465,332)),
    ((839,254), (915,330)),
    ((959,616), (182,616)),
    ((729,723), (487,965)),
    ((64,838), (953,838)),
    ((689,830), (689,982)),
    ((191,83), (191,879)),
    ((522,833), (942,833)),
    ((877,785), (877,346)),
    ((255,95), (556,95)),
    ((782,491), (475,798)),
    ((268,815), (812,271)),
    ((119,181), (905,181)),
    ((445,457), (742,160)),
    ((973,30), (27,976)),
    ((356,681), (356,289)),
    ((882,279), (914,279)),
    ((672,162), (672,153)),
    ((180,729), (357,729)),
    ((985,716), (985,313)),
    ((191,618), (191,963)),
    ((949,749), (636,749)),
    ((289,902), (142,902)),
    ((923,615), (123,615)),
    ((710,929), (541,760)),
    ((211,402), (211,433)),
    ((515,178), (533,178)),
    ((525,869), (525,578)),
    ((201,569), (17,569)),
    ((629,848), (882,848)),
    ((152,512), (152,189)),
    ((914,723), (764,723)),
    ((218,231), (721,734)),
    ((438,382), (846,382)),
    ((582,475), (582,559)),
    ((529,943), (529,683)),
    ((330,312), (59,312)),
    ((242,900), (862,900)),
    ((271,220), (271,118)),
    ((182,459), (182,673)),
    ((513,265), (513,420)),
    ((918,942), (378,942)),
    ((277,765), (812,230)),
    ((625,874), (219,874)),
    ((737,533), (644,626)),
    ((647,975), (152,480)),
    ((638,284), (785,284)),
    ((549,680), (549,877)),
    ((886,278), (372,792)),
    ((130,560), (516,174)),
    ((186,741), (186,555)),
    ((208,536), (469,536)),
    ((674,906), (312,906)),
    ((934,156), (934,322)),
    ((568,412), (214,412)),
    ((243,19), (243,814)),
    ((861,230), (104,987)),
    ((683,891), (683,533)),
    ((545,740), (545,980)),
    ((343,320), (796,320)),
    ((821,220), (821,302)),
    ((578,741), (578,141)),
    ((633,405), (27,405)),
    ((645,975), (225,555)),
    ((25,527), (412,527)),
    ((378,817), (378,913)),
    ((352,741), (352,293)),
    ((48,986), (925,109)),
    ((506,231), (491,231)),
    ((854,883), (48,77)),
    ((261,221), (895,855)),
    ((902,240), (902,943)),
    ((145,338), (770,963)),
    ((832,216), (832,869)),
    ((480,385), (324,385)),
    ((644,202), (433,202)),
    ((202,176), (190,176)),
    ((668,693), (668,349)),
    ((95,230), (143,230)),
    ((873,144), (67,950)),
    ((232,509), (238,509)),
    ((963,43), (133,873)),
    ((527,631), (641,517)),
    ((363,61), (849,61)),
    ((72,326), (72,861)),
    ((542,801), (233,492)),
    ((247,48), (247,785)),
    ((972,563), (480,71)),
    ((362,870), (932,300)),
    ((263,811), (263,584)),
    ((556,157), (417,157)),
    ((946,900), (175,129)),
    ((790,542), (530,542)),
    ((777,195), (154,818)),
    ((71,764), (71,193)),
    ((197,13), (453,13)),
    ((664,714), (158,714)),
    ((257,819), (257,730)),
    ((796,927), (688,927)),
    ((124,53), (954,883)),
    ((30,16), (980,966)),
    ((84,151), (597,151)),
    ((840,776), (684,776)),
    ((548,460), (718,630)),
    ((291,635), (291,151)),
    ((948,43), (58,933)),
    ((373,483), (373,591)),
    ((309,81), (259,81)),
    ((692,808), (692,835)),
    ((737,112), (215,634)),
    ((808,595), (808,115)),
    ((160,912), (973,99)),
    ((494,191), (494,475)),
    ((713,925), (43,255)),
    ((736,580), (290,134)),
    ((257,679), (725,211)),
    ((464,81), (712,81)),
    ((35,147), (35,420)),
    ((372,159), (372,548)),
    ((508,228), (682,402)),
    ((120,491), (518,889)),
    ((139,948), (272,815)),
    ((398,523), (398,818)),
    ((935,50), (40,945)),
    ((415,959), (195,739)),
    ((250,868), (250,930)),
    ((77,60), (917,900)),
    ((584,389), (493,298)),
    ((362,163), (362,704)),
    ((670,740), (670,703)),
    ((689,297), (689,388)),
    ((988,572), (988,340)),
    ((238,248), (238,916)),
    ((748,753), (29,34)),
    ((184,565), (184,486)),
    ((812,217), (812,34)),
    ((60,140), (96,104)),
    ((826,673), (230,673)),
    ((221,221), (207,235)),
    ((449,483), (270,304)),
    ((805,810), (805,564)),
    ((952,52), (139,865)),
    ((428,967), (312,851)),
    ((854,673), (661,673)),
    ((985,209), (853,209)),
    ((523,365), (54,365)),
    ((492,171), (646,171)),
    ((908,853), (69,14)),
    ((38,698), (724,12)),
    ((400,479), (167,479)),
    ((948,313), (948,976)),
    ((280,145), (37,145)),
    ((206,858), (683,381)),
    ((203,413), (545,413)),
    ((726,173), (673,173)),
    ((30,954), (150,954)),
    ((319,592), (870,41)),
    ((808,91), (180,719)),
    ((845,612), (972,485)),
    ((160,430), (160,780)),
    ((19,339), (379,339)),
    ((476,550), (476,291)),
    ((341,785), (229,673)),
    ((371,476), (371,663)),
    ((509,836), (412,933)),
    ((980,20), (31,969)),
    ((822,526), (328,32)),
    ((859,314), (425,314)),
    ((963,961), (963,100)),
    ((984,978), (31,25)),
    ((659,251), (619,211)),
    ((649,477), (846,477)),
    ((32,259), (724,951)),
    ((468,753), (468,91)),
    ((690,301), (690,652)),
    ((436,912), (845,503)),
    ((32,123), (576,667)),
    ((142,79), (741,678)),
    ((610,228), (468,370)),
    ((172,667), (172,736)),
    ((961,700), (132,700)),
    ((804,875), (804,213)),
    ((71,970), (340,970)),
    ((171,52), (149,30)),
    ((754,604), (226,604)),
    ((485,941), (27,941)),
    ((126,383), (328,181)),
    ((41,39), (987,985)),
    ((128,62), (896,830)),
    ((414,278), (923,787)),
    ((712,15), (712,859)),
    ((794,35), (200,629)),
    ((516,147), (402,261)),
    ((526,862), (905,862)),
    ((721,407), (721,887)),
    ((728,920), (339,920)),
    ((117,417), (203,417)),
    ((291,561), (17,835)),
    ((171,359), (837,359)),
    ((93,125), (136,125)),
    ((220,226), (220,177)),
    ((75,434), (75,407)),
    ((235,664), (141,664)),
    ((553,490), (566,477)),
    ((487,651), (487,877)),
    ((699,150), (933,384)),
    ((73,556), (453,556)),
    ((363,371), (363,984)),
    ((905,106), (668,106)),
    ((139,271), (139,125)),
    ((466,379), (466,420)),
    ((12,935), (625,935)),
    ((89,892), (779,892)),
    ((119,701), (270,852)),
    ((354,886), (80,886)),
    ((917,376), (440,376)),
    ((23,182), (794,953)),
    ((451,718), (121,718)),
    ((62,251), (62,451)),
    ((642,74), (642,698)),
    ((425,200), (442,200)),
    ((828,175), (828,405)),
    ((751,743), (591,743)),
    ((569,681), (574,681)),
    ((329,187), (329,837)),
    ((302,592), (302,230)),
    ((359,135), (386,108)),
    ((44,234), (44,731)),
    ((836,305), (836,574)),
    ((170,512), (367,512)),
    ((576,699), (576,44)),
    ((398,185), (821,185)),
    ((733,78), (733,747)),
    ((141,183), (141,787)),
    ((65,360), (65,691)),
    ((828,780), (828,98)),
    ((776,744), (776,751)),
    ((881,74), (481,474)),
    ((438,642), (438,399)),
    ((676,972), (175,972)),
    ((60,318), (56,314)),
    ((312,169), (341,169)),
    ((736,472), (392,128)),
    ((225,281), (164,281)),
    ((407,799), (341,799)),
    ((458,826), (983,301)),
    ((12,988), (987,13)),
    ((23,854), (662,215)),
    ((82,863), (82,416)),
    ((542,708), (542,44)),
    ((659,51), (520,51)),
    ((353,246), (353,90)),
    ((985,976), (77,68)),
    ((628,493), (628,510)),
    ((51,48), (635,48)),
    ((97,814), (828,83)),
    ((14,44), (773,44)),
    ((603,178), (597,178)),
    ((11,220), (783,220)),
    ((613,39), (613,719)),
    ((68,303), (690,925)),
    ((121,974), (896,199)),
    ((343,54), (343,837)),
    ((744,303), (744,942)),
    ((678,370), (246,370)),
    ((937,134), (84,987)),
    ((357,333), (357,516)),
    ((848,212), (429,631)),
    ((909,244), (138,244)),
    ((122,794), (786,130)),
    ((274,611), (57,611)),
    ((66,337), (385,18)),
    ((847,356), (831,356)),
    ((740,480), (740,359)),
    ((194,443), (194,301)),
    ((50,564), (572,42)),
    ((86,587), (774,587)),
    ((708,258), (49,917)),
    ((420,530), (277,387)),
    ((509,580), (509,71)),
    ((237,196), (479,196)),
    ((442,287), (850,287)),
    ((830,393), (532,393)),
    ((274,720), (501,493)),
    ((610,565), (218,957)),
    ((380,393), (380,800)),
    ((237,847), (155,847)),
    ((267,791), (52,791)),
    ((275,772), (275,794)),
    ((239,238), (419,418)),
    ((200,785), (884,101)),
    ((185,980), (185,284)),
    ((47,46), (750,749)),
    ((724,661), (724,337)),
    ((630,349), (666,349)),
    ((21,911), (21,569)),
    ((661,562), (661,925)),
    ((41,898), (41,104)),
    ((988,67), (105,67)),
    ((739,65), (868,65)),
    ((187,973), (809,973)),
    ((730,211), (255,686)),
    ((254,445), (254,872)),
    ((622,364), (235,751)),
    ((402,980), (761,621)),
    ((46,488), (960,488)),
    ((799,708), (799,862)),
    ((909,181), (909,189)),
    ((450,266), (450,304)),
    ((631,584), (631,455)),
    ((164,830), (744,250)),
    ((679,755), (690,744)),
    ((949,26), (190,785)),
    ((695,783), (218,783)),
    ((269,151), (40,151)),
    ((166,152), (22,152)),
    ((281,819), (922,178)),
    ((956,649), (956,593)),
];

