#include <iostream>
#include <cstdlib>
using namespace std;

int main()

{
         cout << "Welcome to The Sorting Manager!!!" <<endl;
         system("gnome-terminal -x sh -c 'sudo apt-get install poppler-utils ; pdftohtml -i Lin*pdf new.html ; sudo apt-get install stack ; sudo apt-get install pandoc ; pandoc -o new.doc *s.html ; pdftotext Lin*pdf new.txt ; pandoc -s -o new.rtf *s.html' ");

         return 0;

}
