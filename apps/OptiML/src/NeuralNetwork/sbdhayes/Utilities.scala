/* 
*
* Author: Saunders Hayes (sbdhayes@cs.stanford.edu)
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*/

import optiml.compiler._
import optiml.library._
import optiml.shared._


/*
* File used to store helper functions for my applications. Needed these functions across multiple applications. 
*/


trait NeuralNetUtilities extends OptiMLApplication {

	//Rounds up from 0.5 and down otherwise. Only for values 0-1. 
	def roundValue(value: Rep[Double]) = {
		if(value >= 0.5) {
			1.0
		} else {
			0.0
		}
	}

	//Prints error message
	def printError(message: Rep[String]) {
		println(message)
		exit(-1)
	}

	//Scales values in a range from range (initMin-initMax) to range (newMin-newMax)
	def scaleValue(value: Rep[Double], initMin: Rep[Double], initMax: Rep[Double], newMin: Rep[Double], newMax: Rep[Double]) = {
		((newMax - newMin) * (value - initMin))/(initMax - initMin) + newMin
	}

	//Tests if two vectors are equal
	def vectorEquals(vec1: Rep[DenseVector[Double]], vec2: Rep[DenseVector[Double]]) = {
		if(vec1.length != vec2.length) {
			false
		} else {
			var index = 0
			var isEqual = true
			while(index < vec2.length) {
				if(vec1(index) != vec2(index)) {
					isEqual = false
				}
				index+=1
			}
			isEqual
		}
	}

}
