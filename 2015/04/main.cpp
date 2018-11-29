#include <iostream>
#include <string>
#include <sstream>
#include "md5.h"

using namespace std;

bool hasFirstFiveZeros(const string& s) {
    return (s[0] == '0') && (s[1] == '0') && (s[2] == '0') && (s[3] == '0') && (s[4] == '0') && (s[5] == '0');
}

int main() {
    string seed;
    stringstream ss;
    ss.str("");
    int i;
    bool done = false;
    getline(cin, seed);
    for(i = 0; !done; i++){
        ss << seed << i;
        done = hasFirstFiveZeros(md5(ss.str()));
        ss.str("");
        ss.clear();
    }
    cout << i << endl;
    return 0;
}
