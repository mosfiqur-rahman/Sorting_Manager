#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main()
{
    string  filename_1 = "new.doc";
    string  filename_2 = "new.txt";
      int index_1;

    string line;

        ifstream file(filename_1);
        getline(file, line);
        //file.close();
        for (int j = 0; j < line.length(); j++)
        {
                if (line[j]  > 64 && line[j]  < 91)
                {
                        index_1 = j;
                        line = line.substr(index_1);
                        break;

                }
                else
                {
                        continue;
                }
        }
                for (int k = 0; k < line.length(); k++)
                {
                        if (line[k]  ==  60)
                        {
                                int index_2 = k;
                                line = line.substr(0 , index_2);
                                cout << line;
                        }
                        else
                        {
                                continue;
                        }
                }


      //system("gnome-terminal -x sh -c 'pandoc -s -o new.pdf new.txt' ");

      rename(filename_2.c_str() , line.c_str() );



    return 0;
}
