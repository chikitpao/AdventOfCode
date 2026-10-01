/*
    AoC 2020, Day 17: Conway Cubes
    Author: Chi-Kit Pao
    
    Outputs:
    Question 1: How many cubes are left in the active state after the sixth cycle?
    Answer: 375
    Question 2: How many cubes are left in the active state after the sixth cycle?
    Answer: 2192
*/


public class ConwayCubes
{
    public void AddPos(int x, int y, int z, int w){
        if (!initialized)
        {
            initialized = true;
            minX = maxX = x;
            minY = maxY = y;
            minZ = maxZ = z;
            minW = maxW = w;
        }
        else
        {
            minX = Math.Min(minX, x);
            minY = Math.Min(minY, y);
            minZ = Math.Min(minZ, z);
            minW = Math.Min(minW, w);
            maxX = Math.Max(maxX, x);
            maxY = Math.Max(maxY, y);
            maxZ = Math.Max(maxZ, z);
            maxW = Math.Max(maxZ, w);
        }
        mySet.Add((x, y, z, w));
    }

    public int CountNeighbors(int x, int y, int z, int w)
    {
        int n = 0;
        for(int nx = x - 1; nx <= x + 1; nx++) 
        {
            for(int ny = y - 1; ny <= y + 1; ny++)
            {
                for(int nz = z - 1; nz <= z + 1; nz++)
                {
                    int lw = w;
                    int uw = w;
                    if(part2)
                    {
                        lw = w - 1;
                        uw = w + 1;
                    }
                    for(int nw = lw; nw <= uw; nw++)
                    {
                        if(nx == x && ny == y && nz == z && nw == w)
                        {
                            continue;
                        }
                        if(mySet.Contains((nx, ny, nz, nw)))
                        {
                            n++;
                        }
                    }
                }
            }
        }
        return n;
    }

    public ConwayCubes RunCycle()
    {
        ConwayCubes newCubes = new();
        newCubes.part2 = part2;
        for(int x = minX - 1; x <= maxX + 1; x++) 
        {
            for(int y = minY - 1; y <= maxY + 1; y++)
            {
                for(int z = minZ - 1; z <= maxZ + 1; z++)
                {
                    int lw = 0;
                    int uw = 0;
                    if(part2)
                    {
                        lw = minW - 1;
                        uw = maxW + 1;
                    }
                    for(int w = lw; w <= uw; w++)
                    {
                        int n = CountNeighbors(x, y, z, w);
                        if(mySet.Contains((x, y, z, w)))
                        {
                            // active
                            if(n == 2 || n == 3)
                            {
                                newCubes.AddPos(x, y, z, w);
                            }
                        }
                        else
                        {
                            // inactive
                            if(n == 3)
                            {
                                newCubes.AddPos(x, y, z, w);
                            }
                        }
                    }
                }
            }
        }
        return newCubes;
    }

    public bool initialized = false;
    public int minX = 0;
    public int minY = 0;
    public int minZ = 0;
    public int minW = 0;
    public int maxX = 0;
    public int maxY = 0;
    public int maxZ = 0;
    public int maxW = 0;
    public bool part2 = false;
    public HashSet<(int x , int y, int z, int w)> mySet = new();
};


public static class Program
{
    public static ConwayCubes ReadInCubes(string fileName)
    {
        ConwayCubes cubes = new();
        int i = 0;
        foreach(string line in File.ReadLines(fileName))
        {
            for (int j = 0; j < line.Length; j++)
            {
                if(line[j] == '#')
                {
                    cubes.AddPos(j, i, 0, 0);
                }
            }
            i += 1;
        }
        return cubes;
    }
    public static ConwayCubes RunCycle(ConwayCubes cc, int cycle)
    {
        ConwayCubes newCubes = cc;
        for(int i = 0; i < cycle; i++)
        {
            newCubes = newCubes.RunCycle(); 
        }
        return newCubes;
    }

    public static void Main()
    {
        ConwayCubes originalCubes = ReadInCubes("input.txt");

        ConwayCubes cubes1 = RunCycle(originalCubes, 6);
        Console.WriteLine("Question 1: How many cubes are left in the active state after the sixth cycle?");
        Console.WriteLine($"Answer: {cubes1.mySet.Count}");
        originalCubes.part2 = true;
        ConwayCubes cubes2 = RunCycle(originalCubes, 6);
        Console.WriteLine("Question 2: How many cubes are left in the active state after the sixth cycle?");
        Console.WriteLine($"Answer: {cubes2.mySet.Count}");
    }
}
