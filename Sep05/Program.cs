Console.WriteLine("Who goes first? 0 = naught, 1 = cross");

var symbol = Console.ReadLine()?.Trim();
var gridState = new string?[3, 3];
bool winnerFound = false;

while (!winnerFound)
{

    Console.WriteLine($"Enter coordinates for your turn {(symbol == "0" ? "Naughts" : "Crosses")} ");
    var lineArgs = Console.ReadLine()?
                          .Split(' ')
                          .Select(int.Parse)
                          .ToArray();

    var turn = (x: lineArgs[0], y: lineArgs[1]);

    if (gridState[turn.x, turn.y] == null)
    {
        gridState[turn.x, turn.y] = symbol == "1" ? "x" : "0";
        winnerFound = IsDiagonalWinner(symbol, gridState) || IsStraightWinner(symbol, gridState);
        RenderGrid(gridState);
        if (winnerFound)
            Console.WriteLine((symbol == "1" ? "Crosses" : "Naughts") + "Wins");
        else
            symbol = symbol == "0" ? "1" : "0";
    }
    else
        Console.WriteLine("Already Taken");
}

void RenderGrid(string?[,] gridState)
{
    var gridRows = Enumerable.Range(0, gridState.GetUpperBound(0) + 1).Reverse().Select(y => string.Join('|', Enumerable.Range(0, gridState.GetUpperBound(1) + 1).Select(x => (gridState[x, y] ?? "-"))));
    Console.Write(string.Join('\n', gridRows));
    Console.WriteLine();
}

bool IsStraightWinner(string symbol, string?[,] gridState)
{
    var across = Enumerable.Range(0, 3)
    .SelectMany(b => Enumerable.Range(0, 3)
        .Select(a => gridState[a, b]))
    .All(x => x == symbol);

    var down = Enumerable.Range(0, 3)
    .SelectMany(b => Enumerable.Range(0, 3)
        .Select(a => gridState[b, a]))
    .All(x => x == symbol);

    return across || down;
}

bool IsDiagonalWinner(string symbol, string?[,] gridState)
{
    var diagUp = Enumerable.Range(0, 3).Select(a => gridState[a, a]).All(a => a == symbol);
    var diagDown = Enumerable.Range(0, 3).Select(a => gridState[2 - a, a]).All(a => a == symbol);
    return diagUp || diagDown;
}

