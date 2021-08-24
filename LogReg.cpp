/*
 LOGISTIC REGRESSION FROM SCRATCH
 
 By: Jeremiah Joseph
 */

#include <iostream>
#include <vector>
#include <fstream>
#include <math.h>
#include <chrono>

using namespace std;

//Function Headers
vector<double> sigmoid (vector<double> z);
vector<double> probVector (double dataMatrix[900][2], vector<double> weights);
vector<double> getError (vector<double> labels, vector<double> prob_vector);
vector<double> updateWeight (vector<double> weights, double learningRate,double dataMatrix[900][2], vector<double> error);


int main(int argc, const char * argv[]) {
    
    // Input file stream
    ifstream inFS;
    string line;
    string x_in, pclass_in, survived_in, sex_in, age_in;
    const int MAX_LEN = 1500;
    
    // vectors for data.
    vector<double> x(MAX_LEN);
    vector<double> pclass(MAX_LEN);
    vector<double> survived(MAX_LEN);
    vector<double> sex(MAX_LEN);
    vector<double> age(MAX_LEN);


    
    // Try to open file
    cout << "Opening file titanic_project.csv." << endl;
    
    inFS.open("titanic_project.csv");
    if (!inFS.is_open()) {
       cout << "Could not open file titanic_project.csv" << endl;
       return 1; // 1 indicates error
    }
    
    cout << "Reading line 1" << endl;
    getline(inFS, line);
    
    // echo heading
    cout << "heading: " << line << endl;
    
    //getting data from file
    int numObservations = 0;
    while (inFS.good()) {
        
        //reading data
        getline(inFS, x_in, ',');
        getline(inFS, pclass_in, ',');
        getline(inFS, survived_in, ',');
        getline(inFS, sex_in, ',');
        getline(inFS, age_in, '\n');
         
        //storing data into vectors
       // x.at(numObservations) = stof(x_in);
        pclass.at(numObservations) = stof(pclass_in);
        survived.at(numObservations) = stof(survived_in);
        sex.at(numObservations) = stof(sex_in);
        age.at(numObservations) = stof(age_in);
        
        
        numObservations++;
    }
    cout << "Closing file titanic_project.csv." << endl;
    inFS.close(); // Done with file, so close it
    
    cout << "Number of records: " << numObservations << endl << endl;
    
    // divide into train and test
    vector<double>::const_iterator first = pclass.begin();
    vector<double>::const_iterator split = pclass.begin() + 900;
    vector<double>::const_iterator last = pclass.begin() + 1046;
    vector<double> pclassTrain(first, split);
    vector<double> pclassTest(split, last);
    
    vector<double>::const_iterator first2 = survived.begin();
    vector<double>::const_iterator split2 = survived.begin() + 900;
    vector<double>::const_iterator last2 = survived.begin() + 1046;
    vector<double> survivedTrain(first2, split2);
    vector<double> survivedTest(split2, last2);
    
    //RUNNING MACHINE LEARNING ALGORITHM
    auto start = std::chrono::high_resolution_clock::now();
    
    //Set up weight vector
    vector<double> weights(2);
    weights[0] = 1;
    weights[1] = 1;
    
    //set up data matrix
    double dataMatrix[pclassTrain.size()][2];
    for (int i = 0; i < pclassTrain.size(); i++) {
        dataMatrix[i][0] = 1;
    }
    for (int i = 0; i < pclassTrain.size(); i++) {
        dataMatrix[i][1] = pclassTrain[i];
    }
    
    //doing gradient descent
    double learningRate = .001;
    //for loop for iterating through gradient descent process process
    for (int i = 0; i < 500000; i++) {
        vector<double> probabilityVector = probVector(dataMatrix, weights);
    
        vector<double> error = getError(survivedTrain, probabilityVector);
   
        //updating weights
        vector<double> newWeights = updateWeight(weights, learningRate, dataMatrix, error);
        weights[0] = newWeights[0];
        weights[1] = newWeights[1];
    
   }
   
    auto stop = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_sec = stop-start;
    cout << "Time:" << elapsed_sec.count() << endl;

    
    cout << "Coefficients: " << weights[0] << ", " << weights[1] << endl << endl;
    
    //TESTING ON TEST DATA
    
    //set up test matrix
    double testMatrix[pclassTest.size()][2];
    for (int i = 0; i < pclassTest.size(); i++) {
        testMatrix[i][0] = 1;
    }
    for (int i = 0; i < pclassTest.size(); i++) {
        testMatrix[i][1] = pclassTest[i];
    }
    
    
    //get predicted value
    vector<double> predicted (146);
    //doing matrix multiplication of testMatrix and weights
    for (int i = 0; i < predicted.size(); i++) {
        predicted[i] = testMatrix[i][0] * weights[0] + testMatrix[i][1] * weights[1];
    }
    
    //getting probabilities
    vector<double> probabilities (146);
    for (int i=0; i < probabilities.size(); i++) {
        probabilities[i] = exp(predicted[i]) / (1 + exp(predicted[i]));
    }
    
    //getting predictions
    vector<double> predictions (146);
    for (int i = 0; i < predictions.size(); i++) {
        if (probabilities[i] > .5) {
            predictions[i] = 1;
        }
        else {
            predictions[i] = 0;
        }
    }
    
    //calculating tp, fp, tn, fn
    double tp = 0.0, fp = 0.0, tn = 0.0, fn = 0.0;
    for(int i = 0; i < predictions.size(); i++) {
        if (predictions[i] == 1 && survivedTest[i] == 1) {
            tn++;
        }
        else if (predictions[i] == 1 && survivedTest[i] == 0) {
            fn++;
        }
        else if (predictions[i] == 0 && survivedTest[i] == 0) {
            tp++;
        }
        else if (predictions[i] == 0 && survivedTest[i] == 1) {
            fp++;
        }
    }
    //calculating and printing accuracy, specficity, and sensitivity
    double accuracy = (tp + tn) / (tp +tn +fp+fn);
    double sensitivity = (tp) / (tp + fn);
    double specificity = (tn) / (tn + fp);
    cout << "Accuracy: " << accuracy << endl;
    cout << "Sensitivity: " << sensitivity << endl;
    cout << "Specificity: " << specificity << endl;
    
    
    
}

//sigmoid function. returns vector.
vector<double> sigmoid (vector<double> z) {
    
    //vector to return
    vector<double> sig(z.size());
    
    //calculating value for each component of z
    for (int i = 0; i < z.size(); i++) {
        sig[i] = 1.0 / (1 + exp(z[i] * -1));
    }
    
    return sig;
}

//Updates to probability vector
vector<double> probVector (double dataMatrix[900][2], vector<double> weights) {
    //Multiplying matrix
    vector<double> z (900);
    for (int i = 0; i < z.size(); i++) {
        z[i] = dataMatrix[i][0] * weights[0] + dataMatrix[i][1] * weights[1];
    }
    
    vector<double> sig = sigmoid(z);
    return sig;
    
}

//Gets to error
vector<double> getError (vector<double> labels, vector<double> prob_vector) {
    vector<double> error (900);
    //subtracting vectors
    for (int i = 0; i < error.size(); i++) {
        error[i] = labels[i] - prob_vector[i];
    }
    return error;
}

//Function updates the weights
vector<double> updateWeight (vector<double> weights, double learningRate,double dataMatrix[900][2], vector<double> error) {
    vector<double> newWeights(2);
    
    //t(dataMatrix) %*% error matrix multiplication
    for (int i = 0; i < newWeights.size(); i++) {
        double temp = 0;
        for (int j = 0; j < 900; j++) {
            temp += dataMatrix[j][i] * error[j];
        }
        newWeights[i] = temp;
    }
    
    //Multiplying weights by learning rate
    newWeights[0] *= learningRate;
    newWeights[1] *= learningRate;
    
    //adding by old weights
    newWeights[0] += weights[0];
    newWeights[1] += weights[1];
    
    return newWeights;
}
