#include <queue>
#include <matchingR.h>

// [[Rcpp::depends(RcppArmadillo)]]

#include "main.h"

//' Compute the Gale-Shapley Algorithm
//'
//' This function computes the Gale-Shapley Algorithm with one-to-one matching.
//' This function requires very specific types of arguments. It might be more
//' convenient to call the function \code{one2one()} instead that allows for
//' more flexible input choices.
//'
//' @param proposerPref is a matrix with the preference order of the proposing side of 
//' the market
//' @param reviewerUtils is a matrix with cardinal utilities of the courted side of the 
//' market
//' @return A list with the successful proposals and engagements. 
//' \code{proposals} is a vector whose nth element contains the id of the reviewer 
//' that proposer n is matched to. 
//' \code{engagements} is a vector whose nth element contains the id of the proposer 
//' that reviewer n is matched to.  
// [[Rcpp::export]]
List galeShapleyMatching(const umat proposerPref, const mat reviewerUtils) {
    
    // number of proposers (men)
    int M = proposerPref.n_rows;
    // number of reviewers (women)
    int N = proposerPref.n_cols;
    // initialize engagements, proposals
    vec engagements(N), proposals(M);
    // create an integer queue of bachelors
    queue<int> bachelors;
    // set all proposals to N (aka no proposals)
    proposals.fill(N);
    // set all engagements to M (aka no engagements)
    engagements.fill(M);
    // every proposer starts out as a bachelor
    for(int iX=M-1;iX>=0;iX--) {
        bachelors.push(iX);
    }
    
    // loop until there are no proposals to be made
    while (!bachelors.empty()) {
        // get the index of the proposer
        int proposer = bachelors.front();
        // get the proposer's preferences
        urowvec proposerPrefrow = proposerPref.row(proposer);
        // find the best available match
        for(int jX=0;jX<N;jX++) {
            // index of the reviewer that the proposer is interested in
            int wX = proposerPrefrow(jX);
            // check if wX is available
            if(engagements(wX)==M) {
                engagements(wX) = proposer;
                proposals(proposer) = wX;
                break;
            }
            // check if the wX can be poached
            if(reviewerUtils(wX, proposer) > reviewerUtils(wX, engagements(wX))) {
                // make the guy who was just dropped a bachelor again
                proposals(engagements(wX)) = N;
                // and put him back into the bachelor queue
                bachelors.push(engagements(wX));
                // hook up
                engagements(wX) = proposer;
                proposals(proposer) = wX;
                break;
            }
        }
        // pop at the end
        bachelors.pop();         
    }
    
    return List::create(
      _["proposals"]   = proposals,
      _["engagements"] = engagements);
}

//' Sort indices of a matrix within row
//' 
//' Within each row of a matrix, this function returns the indices of each 
//' element in descending order
//' 
//' @param u is the input matrix
//' @return a matrix with sorted indicies
//' 
// [[Rcpp::export]]
umat sortIndex(const mat u) {
    int N = u.n_cols;
    int M = u.n_rows;
    umat sortedIdx(M,N);
    for(int jX=0;jX<M;jX++) {
        sortedIdx.row(jX) = sort_index(u.row(jX), "descend");
    }
    return sortedIdx;
}

//' Rank elements within row of a matrix
//' 
//' This function returns the rank of each element within each row of a matrix.
//' The highest element receives the highest rank.
//' 
//' @param sortedIdx is the input matrix
//' @return a rank matrix
//' 
// [[Rcpp::export]]
umat rankIndex(const umat sortedIdx) {
    int N = sortedIdx.n_cols;
    int M = sortedIdx.n_rows;
    umat rankedIdx(M,N);
    for(int jX=0; jX<M; jX++) {
        for(int iX=0; iX<N; iX++) {
            rankedIdx.at(jX, sortedIdx.at(jX,iX)) = iX;
        }
    }
    return rankedIdx;
}

//' Check if a matching is stable
//'
//' This function checks if a given matching is stable for a particular set of
//' preferences. This function can check if a given check one-to-one, 
//' one-to-many, or many-to-one matching is stable.
//'
//' @param proposerUtils is a matrix with cardinal utilities of the proposing side of the 
//' market
//' @param reviewerUtils is a matrix with cardinal utilities of the courted side of the 
//' market
//' @param proposals is a matrix that contains the id of the reviewer that a given
//' proposer is matched to: the first row contains the id of the reviewer that is 
//' matched with the first proposer, the second row contains the id of the reviewer 
//' that is matched with the second proposer, etc. The column dimension accommodates
//' proposers with multiple slots.
//' @param engagements is a matrix that contains the id of the proposer that a given
//' reviewer is matched to. The column dimension accommodates reviewers with multiple
//' slots
//' @return true if the matching is stable, false otherwise
// [[Rcpp::export]]
bool checkStability(mat proposerUtils, mat reviewerUtils, umat proposals, umat engagements) {

    // number of workers
    const int M = proposerUtils.n_rows;
    // number of firms
    const int N = proposerUtils.n_cols;
    // number of slots per firm
    const int slotsReviewers = engagements.n_cols;
    // number of slots per worker
    const int slotsProposers = proposals.n_cols;
    
    // turn proposals into C++ indices 
    proposals = proposals-1;
    // turn engagements into C++ indices
    engagements = engagements-1;
        
    // more jobs than workers (add utility from being unmatched to firms' preferences)
    if(N*slotsReviewers>M*slotsProposers) {
        reviewerUtils.insert_cols(M, 1);
        reviewerUtils.col(M).fill(-1e10);
    }
    // more workers than jobs (add utility from being unmatched to workers' preferences)
    if(M*slotsProposers>N*slotsReviewers) {
        proposerUtils.insert_cols(N, 1);
        proposerUtils.col(N).fill(-1e10);
    }
    // loop over workers
    for(int wX=0; wX<M; wX++) {
        // loop over firms
        for(int fX=0; fX<N; fX++) {
            // loop over multiple "slots" at the same worker
            for(int swX=0;swX<slotsProposers;swX++) {
                // loop over multiple slots at the same firm
                for(int sfX=0;sfX<slotsReviewers;sfX++) {
                    // check if wX and fX would rather be matched with each other than with their actual matches
                    if(reviewerUtils(fX, wX) > reviewerUtils(fX, engagements(fX, sfX)) && proposerUtils(wX, fX) > proposerUtils(wX, proposals(wX, swX))) {
                        Rprintf("matching is not stable; worker %d would rather be matched to firm %d and vice versa.\n", wX, fX);
                        return false;
                    } 
                }
            }
        }
    }
    return true;
}

