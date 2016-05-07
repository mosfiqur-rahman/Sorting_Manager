#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

using namespace std;


int main()
{
   int index;
   string line, ToParse = "The Linux Command Line";
   string pattern, match;
   istringstream StrStream(ToParse);
 while(StrStream >> line)
    {
        //cout << endl << line << endl;
        ifstream myfile("computer.txt");
       while ( getline(myfile, match))
        {
                if (line == match)
                {
                        cout << match << endl;
                        cout << "matched!!";
                       break;
                }
                else
                {
                      continue;
                }
        }
    }
    return 0;
}
