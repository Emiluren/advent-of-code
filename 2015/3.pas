program aoc3;

type
vec2 = record
    x: integer;
    y: integer;
end;

function hasVisited(const v: vec2; const visitedPositions: array of vec2): boolean;
var
    v2: vec2;
begin
    hasVisited := false;
    for v2 in visitedPositions do begin
        hasVisited := (v2.x = v.x) and (v2.y = v.y);
        if hasVisited then exit;
    end;
end;

var
    inputFile: file of char;
    pos: vec2;
    visitedPositions: array of vec2;
    c: char;
    posSanta: vec2;
    posRobo: vec2;

begin
    assign(inputFile, '3input');
    reset(inputFile);

    pos.x := 0;
    pos.y := 0;
    setLength(visitedPositions, 1);
    visitedPositions[0] := pos;

    while not eof(inputFile) do
    begin
        read(inputFile, c);
        case c of
          '>': pos.x += 1;
          '<': pos.x -= 1;
          '^': pos.y += 1;
          'v': pos.y -= 1;
        end;

        if not hasVisited(pos, visitedPositions) then
        begin
            setLength(visitedPositions, length(visitedPositions) + 1);
            visitedPositions[length(visitedPositions) - 1] := pos;
        end;
    end;

    writeln('Part 1: ', length(visitedPositions));

    reset(inputFile);
    posSanta.x := 0;
    posSanta.y := 0;
    posRobo.x := 0;
    posRobo.y := 0;
    setLength(visitedPositions, 1);
    visitedPositions[0] := posSanta;

    while not eof(inputFile) do
    begin
        read(inputFile, c);
        case c of
          '>': posSanta.x += 1;
          '<': posSanta.x -= 1;
          '^': posSanta.y += 1;
          'v': posSanta.y -= 1;
        end;

        if not hasVisited(posSanta, visitedPositions) then
        begin
            setLength(visitedPositions, length(visitedPositions) + 1);
            visitedPositions[length(visitedPositions) - 1] := posSanta;
        end;

        read(inputFile, c);
        case c of
          '>': posRobo.x += 1;
          '<': posRobo.x -= 1;
          '^': posRobo.y += 1;
          'v': posRobo.y -= 1;
        end;

        if not hasVisited(posRobo, visitedPositions) then
        begin
            setLength(visitedPositions, length(visitedPositions) + 1);
            visitedPositions[length(visitedPositions) - 1] := posRobo;
        end;
    end;

    writeln('Part 2: ', length(visitedPositions));

    close(inputFile);
end.
