/*
 *  findSeqs.cpp
 *  Copyright (C) 2014, Daniel Muenz
 *
 *  Try to abbreviate integer variables by finding arithmetic
 *  subsequences with constant difference 1.  For example, if
 *  the variable has values 1,3,4,5,6,8,9, we abbreviate this
 *  to 1,3:6,8:9. The input vector x must already be sorted
 *  and de-duplicated.
 *
 */

#include <Rcpp.h>
using namespace Rcpp;

std::string newEntry(int start_k, int prev_k) {
    if (prev_k == start_k) {
        return std::to_string(prev_k);
    } else {
        std::string str = std::to_string(start_k);
        str.append(":");
        str.append(std::to_string(prev_k));
        return str;
    }
}

// [[Rcpp::export]]
CharacterVector findSeqs(IntegerVector x) {
    int start_k = x[0], prev_k = x[0];
    int nx = x.size();
    CharacterVector vals1(nx);
    int k;
    int count = 0;
    
    for (int i = 1; i < nx; i++) {
        k = x[i];
        if (k != prev_k + 1) {
            vals1[count++] = newEntry(start_k, prev_k);
            start_k = k;
        }
        prev_k = k;
    }
    vals1[count++] = newEntry(start_k, prev_k);

    CharacterVector vals2(count);
    for (int i = 0; i < count; i++)
        vals2[i] = vals1[i];

    return vals2;
}


