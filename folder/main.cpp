#include <iostream>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>

using namespace std;

int main(const char *path, mode_t mode)
{
    int status;
    string dir1 ,dir2;
     string dirName = "/home/mosfiqur/Desktop";
     dirName = dirName + "/Computer/Linux";
     status = mkdir(dirName.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

     dir1 = "/home/mosfiqur/Desktop/Hackathon/Sorting-Manager/The Linux Command Line";

     dir2 = dirName + "/The Linux Command Line";

     rename(dir1.c_str(), dir2.c_str());
    return 0;
}
