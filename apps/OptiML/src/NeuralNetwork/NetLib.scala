//  
//	Stanford PPL
//  http://ppl.stanford.edu/
//

import optiml.compiler._
import optiml.library._
import optiml.shared._

trait NetLib extends OptiMLApplication {

	// Calculate and return cross entropy
	// Objective functions usually try to maximize this
	// (Or more commonly, to minimize J = -CE + weight decay)
	def cross_entropy(softmax_out: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]]) = {

		val CE_Matrix = (0::softmax_out.numRows, 0::softmax_out.numCols) { (i,j) =>
			val prob = softmax_out(i,j)
			if (y(i) == j) log(prob) else 0.0
		}

		// TODO: Time against this:
		//val CE_Vector = (0::softmax_out.numRows) { i => log(softmax_out(i,y(i))) }

		CE_Matrix.sum
	}

	// Activation functions
	def logistic(z: Rep[Double]) : Rep[Double] = {
		1.0 / (1.0 + exp(z * -1.0))
	}
	def ReLU(z: Rep[Double]) : Rep[Double] = {
		if (z <= 0) 0.0
		else z
	}
	def ReLU_Deriv(z: Rep[Double]) : Rep[Double] = {
		if (z <= 0) 0.0 else 1.0
	}

	// Forward through fully connected layer
	def fullycon_ff(X: Rep[DenseMatrix[Double]], W: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]]) = {
		(0::X.numRows, 0::W.numCols) { (r,c) =>
			val s = sum(0, X.numCols) { i =>
				X(r,i)*W(i,c)
			}
			s + b(c)
		}
	}

	// Forward through fully connected layer
	def fullycon_ff_RNN(X: Rep[DenseMatrix[Double]], H: Rep[DenseMatrix[Double]], Wx: Rep[DenseMatrix[Double]], 
		b: Rep[DenseVector[Double]], Wh: Rep[DenseMatrix[Double]]) = {

		// TODO: Make a BLAS version of this function: H*Wh + X*Wc
		// Note: 
		// X.numRows = H.numRows

		(0::X.numRows, 0::H.numCols) { (r,c) =>
			val s1 = sum(0, X.numCols) { i =>
				X(r,i)*Wx(i,c)
			}
			val s2 = sum(0, H.numCols) { i =>
				H(r,i)*Wh(i,c)
			}
			s1 + s2 + b(c)
		}
	}

	// Forward through fully connected layer
	// BLAS version
	def fullycon_ff_blas(X: Rep[DenseMatrix[Double]], W: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]]) = {

/* Implementation 1: (Replicate may produce unnecessary copy?) 
		val num_samples = X.numRows
		X*W + b.replicate(num_samples, 1)
*/		
/* Alternate Implementation:
		val prod = X*W
		(0::prod.numRows, *) { r => prod(r) + b }
*/

/* Alternate Implementation:
*/
		val prod = X*W
		(0::prod.numRows, 0::prod.numCols) { (r,c) => prod(r,c) + b(c) }
	}

	// Forward through softmax
	def softmax_ff(X: Rep[DenseMatrix[Double]], W: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]]) = {
		val prod = (0::X.numRows, 0::W.numCols) { (r,c) =>
			val s = sum(0, X.numCols) { i =>
				X(r,i)*W(i,c)
			}
			s + b(c)
		}
		val Py_given_X = ( prod ).map(e => exp(e)) // exp(_)
		val sum_of_rows = (0::X.numRows) {
			r => Py_given_X.getRow(r).sum
		}	
		(0::X.numRows, 0::W.numCols) { (i,j) =>
			Py_given_X(i,j)/sum_of_rows(i)
		}
	}

	// Forward through softmax
	// BLAS version
	def softmax_ff_blas(X: Rep[DenseMatrix[Double]], W: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]]) = {

/* Implementation 1: (Replicate may produce unnecessary copy?) 		
		val num_samples = X.numRows
		val prod = X*W + b.replicate(num_samples, 1)
*/
/* Alternate Implementation:
		val prod_no_bias = X*W
		val prod = (0::prod_no_bias.numRows, *) { r => prod_no_bias(r) + b }
*/

/* Alternate Implementation:
*/
		val prod_no_bias = X*W
		val prod = (0::prod_no_bias.numRows, 0::prod_no_bias.numCols) { (r,c) =>
			prod_no_bias(r,c) + b(c)
		}
		val Py_given_X = ( prod ).map(e => exp(e)) // exp(_)
		val sum_of_rows = (0::X.numRows) {
			r => Py_given_X.getRow(r).sum
		}	
		(0::X.numRows, 0::W.numCols) {
			(i,j) => Py_given_X(i,j)/sum_of_rows(i)
		}
	}

	def get_classification_error(all_predictions: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]]) = {

		val num_test = y.length

		// Following the softmax, last step is to select best value from each category
		val predictions = (0::num_test) { r => all_predictions.getRow(r).maxIndex }

		// Compare to the actual classes and print result
		val results = (0::predictions.length) { i =>
			if (predictions(i) == y(i)) 0 else 1
		}
		results.sum.toDouble / num_test.toDouble * 100
	}

	def print_classification_error(all_predictions: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]]) = {

		val classification_error = get_classification_error(all_predictions, y)
		print("Classification Error (")
		print(y.length)
		print(" examples):  ")
		print(classification_error)
		println(" %")
	}

	// Convert matrix with really long rows to a matrix of matrices
	// Used only for visualization, etc., not actual performance code
	def reshape_2D_Tensor_to_4D_Tensor(T_2D: Rep[DenseMatrix[Double]], num_feature_maps: Rep[Int],
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		feature_map_size: Rep[Int],  // = T_2D.numCols / num_feature_maps
		matrix_side_length: Rep[Int] // = sqrt(feature_map_size.toDouble).toInt

	) = {
		(0::T_2D.numRows, 0::num_feature_maps) { (r,c) =>
			(0::matrix_side_length, 0::matrix_side_length) {
				(i,j) => T_2D(r, c*feature_map_size + i*matrix_side_length + j)
			}
		}
	}

	// Convert matrix with really long rows to a matrix of matrices
	// Used only for visualization, etc., not actual performance code
	def reshape_4D_Tensor_to_2D_Tensor(T_4D: Rep[DenseMatrix[DenseMatrix[Double]]],
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		sub_matrix_r: Rep[Int], // = T_4D(0,0).numRows
		sub_matrix_c: Rep[Int], // = T_4D(0,0).numCols
		sub_matrix_total_size: Rep[Int], // = sub_matrix_r * sub_matrix_c
		num_cols: Rep[Int] // = T_4D.numCols * sub_matrix_total_size
	) = {
		(0::T_4D.numRows, 0::num_cols) { (r,c) =>
			// Get the right sub-matrix
			val matrix = T_4D(r,c / sub_matrix_total_size)
			// Get the right element of this sub-matrix
			matrix((c%sub_matrix_total_size)/sub_matrix_c, (c%sub_matrix_total_size)%sub_matrix_c)
		}
	}

	// NOTE: This uses zero-padding, as in AlexNet, so no information is thrown away
	// and the feature map size is unchanged by convolution.
	// This can instead be specified as an option (how much to zero-pad), which may
	// provide some speedups because:
	// - Doesn't get much worse in terms of accuracy
	// - Less computation
	// - Convolutions further reduce fmap size
	// - Easier BackProp (ignore borders)
	// - Prevents divergence on GPU (esp. for small fmaps where borders dominate)
	def conv_ff(X: Rep[DenseMatrix[Double]], num_feature_maps_L1: Rep[Int],
 		W: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]], 
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
 		num_feature_maps_L2: Rep[Int], // = W.numCols
 		num_2D_kernel_elements: Rep[Int], // = W.numRows / num_feature_maps_L1 // Square
 		kernel_size: Rep[Int], // = sqrt(num_2D_kernel_elements.toDouble).toInt
 		half_convolution_length: Rep[Int], // = (kernel_size)/2

		feature_map_total_size: Rep[Int], // = X.numCols / num_feature_maps_L1
		feature_map_size: Rep[Int] // = sqrt(feature_map_total_size.toDouble).toInt
	) = {

		val num_samples = X.numRows
		(0::num_samples, 0::(num_feature_maps_L2*feature_map_total_size)) { (r,c) =>

			// Get the right kernel. One kernel / L2 fmap, so get the fmap number
			val L2_feature_map_num = c/feature_map_total_size
			// We also know exactly the x/y coordinate of this L2 fmap
			val fr = (c%feature_map_total_size) / feature_map_size
			val fc = (c%feature_map_total_size) % feature_map_size
			// There is a different kernel for each source map (in L1) (so while
			// we share kernel weights within an F map, the weights are different
			// for each L1 fmap --> L2 fmap connection)
			// Create a vector which will store the convolution result from each 
			val conv_result_from_each_L1_feature_map = (0::num_feature_maps_L1) { i =>
				val index_to_right_fmap = i*feature_map_total_size
				val index_to_right_kernel = i*num_2D_kernel_elements
				val partial_convolution_result = (0::kernel_size, 0::kernel_size) { (kr, kc) =>
					val kernel_value = W(index_to_right_kernel + kernel_size*kr + kc, L2_feature_map_num)
					val value_from_src_matrix = {
						// Check if out of bounds
						val row_of_src_f_map = fr - half_convolution_length + kr
						val col_of_src_f_map = fc - half_convolution_length + kc
						if (row_of_src_f_map <  0 || row_of_src_f_map >= feature_map_size ||
							col_of_src_f_map <  0 || col_of_src_f_map >= feature_map_size) {
							0.0
						}
						else {
							X(r, index_to_right_fmap + row_of_src_f_map*feature_map_size + col_of_src_f_map)
						}
					}
					kernel_value * value_from_src_matrix
				}
				partial_convolution_result.sum
			}
			conv_result_from_each_L1_feature_map.sum + b(L2_feature_map_num)
		}
	}

	// Backprop through conv layer
	// Returns gradient of cost function wrt the convolutional WEIGHTS
	def conv_bp
	(
		k_total_size: Rep[Int], L1_num_fmaps: Rep[Int], L2_num_fmaps: Rep[Int],
		L1: Rep[DenseMatrix[Double]], downsampled_dJ_dL2: Rep[DenseMatrix[Double]],
		m: Rep[Int], L2_upsample_map: Rep[DenseMatrix[Int]],  // "m" is pooling size
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		k_size: Rep[Int], // = sqrt(k_total_size.toDouble).toInt
		half_k_size: Rep[Int], // = (k_size)/2

		fmap_total_size: Rep[Int], // = L1.numCols / L1_num_fmaps
		fmap_size: Rep[Int], // = sqrt(fmap_total_size.toDouble).toInt
		fmap_total_size_downsampled: Rep[Int], // = downsampled_dJ_dL2.numCols / L2_num_fmaps
		fmap_size_downsampled: Rep[Int] // = fmap_size / m
 	) = {
 
		val num_examples = L1.numRows
		(0::(k_total_size * L1_num_fmaps), 0::L2_num_fmaps) { (r,c) =>

			val L1_fmap_num = r/k_total_size // i
			val L2_fmap_num = c // j
			val k_index = r%k_total_size
			val k_offset_r = (k_index / k_size) - half_k_size
			val k_offset_c = (k_index % k_size) - half_k_size

			val idx_of_L1fmap = L1_fmap_num*fmap_total_size
			val idx_of_L2fmap = L2_fmap_num*fmap_total_size_downsampled

			sum(0, num_examples) { ex =>

				val conv = (0::fmap_size_downsampled, 0::fmap_size_downsampled) { (cr, cc) =>

					// Find the index of the nonzero in this mxm (downsampled) grid
					val nonzero_idx_in_this_mxm =  L2_upsample_map(ex,
						  ( idx_of_L2fmap +
							// Get to the right mxm region for this L2 feature map
							cr*fmap_size_downsampled + 
							cc ).toInt
						)

					// Now we know where in the mxm our nonzero is, so get the upsampled
					// analogue for these downsampled coordinates
					val r_upsampled = cr*m + nonzero_idx_in_this_mxm/m
					val c_upsampled = cc*m + nonzero_idx_in_this_mxm%m

					// Finally, consider shifts
					val shifted_r_upsampled = r_upsampled + k_offset_r
					val shifted_c_upsampled = c_upsampled + k_offset_c

					// Have to shift because we need to convolve dJ/dL2 by moving it
					// around L1.t until we get a kxk kernel back
					if (shifted_c_upsampled < 0 || shifted_c_upsampled >= fmap_size ||
						shifted_r_upsampled < 0 || shifted_r_upsampled >= fmap_size) {
						0.0
					}
					else {

						// Note: This only works because we know the 2 fmaps are the same size
						// So if the result of the convolution is a 1x1, we just want to convolve
						// them when they fully overlap, but this is the same as saying when their
						// top left corners overlap (i.e. no need to align their centers)
						val L1_elem = L1 (ex,  // Current example
							  ( idx_of_L1fmap + // Get to the right L1 feature map
								// Get to the right row / col of this L1 fmap
								shifted_r_upsampled*fmap_size +
								shifted_c_upsampled
								).toInt
							)
						val dJ_dL2_elem = downsampled_dJ_dL2 (ex,  // Current example
							  ( idx_of_L2fmap + // Current L2 feature map
								// Get to the right mxm region for this L2 feature map
								cr*fmap_size_downsampled + 
								cc ).toInt
							)
						(dJ_dL2_elem * L1_elem).toDouble
					}
				}
				conv.toDouble.sum
			}
		}
	}

	// Same as above but without pooling
	def conv_bp_NO_POOL
	(
		k_total_size: Rep[Int], L1_num_fmaps: Rep[Int], L2_num_fmaps: Rep[Int],
		L1: Rep[DenseMatrix[Double]], downsampled_dJ_dL2: Rep[DenseMatrix[Double]],

		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		k_size: Rep[Int], // = sqrt(k_total_size.toDouble).toInt
		half_k_size: Rep[Int], // = (k_size)/2

		fmap_total_size: Rep[Int], // = L1.numCols / L1_num_fmaps
		fmap_size: Rep[Int], // = sqrt(fmap_total_size.toDouble).toInt
		fmap_total_size_downsampled: Rep[Int], // = downsampled_dJ_dL2.numCols / L2_num_fmaps
		fmap_size_downsampled: Rep[Int] // = fmap_size / m
 	) = {
 

 		val m = 1
		val num_examples = L1.numRows
		(0::(k_total_size * L1_num_fmaps), 0::L2_num_fmaps) { (r,c) =>

			val L1_fmap_num = r/k_total_size // i
			val L2_fmap_num = c // j
			val k_index = r%k_total_size
			val k_offset_r = (k_index / k_size) - half_k_size
			val k_offset_c = (k_index % k_size) - half_k_size

			val idx_of_L1fmap = L1_fmap_num*fmap_total_size
			val idx_of_L2fmap = L2_fmap_num*fmap_total_size_downsampled

			sum(0, num_examples) { ex =>

				val conv = (0::fmap_size_downsampled, 0::fmap_size_downsampled) { (cr, cc) =>

					// Find the index of the nonzero in this mxm (downsampled) grid
					val nonzero_idx_in_this_mxm = 0

					// Now we know where in the mxm our nonzero is, so get the upsampled
					// analogue for these downsampled coordinates
					val r_upsampled = cr*m + nonzero_idx_in_this_mxm/m
					val c_upsampled = cc*m + nonzero_idx_in_this_mxm%m

					// Finally, consider shifts
					val shifted_r_upsampled = r_upsampled + k_offset_r
					val shifted_c_upsampled = c_upsampled + k_offset_c

					// Have to shift because we need to convolve dJ/dL2 by moving it
					// around L1.t until we get a kxk kernel back
					if (shifted_c_upsampled < 0 || shifted_c_upsampled >= fmap_size ||
						shifted_r_upsampled < 0 || shifted_r_upsampled >= fmap_size) {
						0.0
					}
					else {

						// Note: This only works because we know the 2 fmaps are the same size
						// So if the result of the convolution is a 1x1, we just want to convolve
						// them when they fully overlap, but this is the same as saying when their
						// top left corners overlap (i.e. no need to align their centers)
						val L1_elem = L1 (ex,  // Current example
							  ( idx_of_L1fmap + // Get to the right L1 feature map
								// Get to the right row / col of this L1 fmap
								shifted_r_upsampled*fmap_size +
								shifted_c_upsampled
								).toInt
							)
						val dJ_dL2_elem = downsampled_dJ_dL2 (ex,  // Current example
							  ( idx_of_L2fmap + // Current L2 feature map
								// Get to the right mxm region for this L2 feature map
								cr*fmap_size_downsampled + 
								cc ).toInt
							)
						(dJ_dL2_elem * L1_elem).toDouble
					}
				}
				conv.toDouble.sum
			}
		}
	}

	// TODO: All these fw prop and bp methods just do convolution,
	// should combine the functionality to just call a "conv" method

	// Backprop through conv layer
	// Returns gradient of cost function wrt the INPUT FEATURE MAPS
	def bp_through_conv_layer
	(
		w: Rep[DenseMatrix[Double]], L1_num_fmaps: Rep[Int],
		downsampled_dJ_dL2: Rep[DenseMatrix[Double]],
		m: Rep[Int], L2_upsample_map: Rep[DenseMatrix[Int]], // "m" is pooling size
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		L2_num_fmaps: Rep[Int], // = w.numCols
		k_total_size: Rep[Int], // = w.numRows / L1_num_fmaps
		k_size: Rep[Int], // = sqrt(k_total_size.toDouble).toInt

		fmap_total_size_downsampled: Rep[Int], // = downsampled_dJ_dL2.numCols / L2_num_fmaps
		fmap_size_downsampled: Rep[Int], // = sqrt(fmap_total_size_downsampled.toDouble).toInt

		fmap_total_size_upsampled: Rep[Int], // = fmap_size_upsampled*fmap_size_upsampled
		fmap_size_upsampled: Rep[Int], // = fmap_size_downsampled*m
		half_L1_fmap_size: Rep[Int], // = (fmap_size_upsampled.toDouble)/2.0

		centering_offset: Rep[Double] // = ((k_size - fmap_size_upsampled).toDouble)/2.0
	) = {
  
		val num_examples = downsampled_dJ_dL2.numRows

		// dJ/dxi = sum_j(dJ/dhj * wij.t)
		(0::num_examples, 0::(L1_num_fmaps * fmap_total_size_upsampled)) { (r, c) =>

			val L1_fmap_num = c/fmap_total_size_upsampled // i
			val L1_fmap_r = (c%fmap_total_size_upsampled) / fmap_size_upsampled
			val L1_fmap_c = (c%fmap_total_size_upsampled) % fmap_size_upsampled
			val L1_fmap_offset_r = L1_fmap_r - (centering_offset + half_L1_fmap_size).toInt
			val L1_fmap_offset_c = L1_fmap_c - (centering_offset + half_L1_fmap_size).toInt

			sum(0, L2_num_fmaps) { L2_fmap_num =>

				val conv = (0::fmap_size_downsampled, 0::fmap_size_downsampled) { (cr, cc) =>

					// Find the index of the nonzero in this mxm (downsampled) grid
					val nonzero_idx_in_this_mxm =  L2_upsample_map(r,
						  ( L2_fmap_num*fmap_total_size_downsampled +
							// Get to the right mxm region for this L2 feature map
							cr*fmap_size_downsampled + 
							cc ).toInt
						)

					// Now we know where in the mxm our nonzero is, so get the upsampled
					// analogue for these downsampled coordinates
					val r_upsampled = cr*m + nonzero_idx_in_this_mxm/m
					val c_upsampled = cc*m + nonzero_idx_in_this_mxm%m

					// Finally, consider shifts
					val shifted_r_upsampled = r_upsampled - L1_fmap_offset_r
					val shifted_c_upsampled = c_upsampled - L1_fmap_offset_c

					// Have to shift because we need to convolve dJ/dL2 by moving it
					// around L1.t until we get a kxk kernel back
					if (shifted_c_upsampled < 0 || shifted_c_upsampled >= k_size ||
						shifted_r_upsampled < 0 || shifted_r_upsampled >= k_size) {
						0.0
					}
					else {
						val k_elem = w ((L1_fmap_num*k_total_size +
								(k_size-1-shifted_r_upsampled)*k_size + (k_size-1-shifted_c_upsampled)).toInt,
								L2_fmap_num)
						val dJ_dL2_elem = downsampled_dJ_dL2 (r,  // Current example
							  ( L2_fmap_num*fmap_total_size_downsampled + // Current L2 feature map
								// Get to the right mxm region for this L2 feature map
								cr*fmap_size_downsampled + 
								cc ).toInt
							)
						(dJ_dL2_elem * k_elem).toDouble
					}
				}
				conv.toDouble.sum
			}
		}
	}

	// Same as above but without pooling
	def bp_through_conv_layer_NO_POOL
	(
		w: Rep[DenseMatrix[Double]], L1_num_fmaps: Rep[Int],
		downsampled_dJ_dL2: Rep[DenseMatrix[Double]],

		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		L2_num_fmaps: Rep[Int], // = w.numCols
		k_total_size: Rep[Int], // = w.numRows / L1_num_fmaps
		k_size: Rep[Int], // = sqrt(k_total_size.toDouble).toInt

		fmap_total_size_downsampled: Rep[Int], // = downsampled_dJ_dL2.numCols / L2_num_fmaps
		fmap_size_downsampled: Rep[Int], // = sqrt(fmap_total_size_downsampled.toDouble).toInt

		fmap_total_size_upsampled: Rep[Int], // = fmap_size_upsampled*fmap_size_upsampled
		fmap_size_upsampled: Rep[Int], // = fmap_size_downsampled*m
		half_L1_fmap_size: Rep[Int], // = (fmap_size_upsampled.toDouble)/2.0

		centering_offset: Rep[Double] // = ((k_size - fmap_size_upsampled).toDouble)/2.0
	) = {
  
		val m = 1
		val num_examples = downsampled_dJ_dL2.numRows

		// dJ/dxi = sum_j(dJ/dhj * wij.t)
		(0::num_examples, 0::(L1_num_fmaps * fmap_total_size_upsampled)) { (r, c) =>

			val L1_fmap_num = c/fmap_total_size_upsampled // i
			val L1_fmap_r = (c%fmap_total_size_upsampled) / fmap_size_upsampled
			val L1_fmap_c = (c%fmap_total_size_upsampled) % fmap_size_upsampled
			val L1_fmap_offset_r = L1_fmap_r - (centering_offset + half_L1_fmap_size).toInt
			val L1_fmap_offset_c = L1_fmap_c - (centering_offset + half_L1_fmap_size).toInt

			sum(0, L2_num_fmaps) { L2_fmap_num =>

				val conv = (0::fmap_size_downsampled, 0::fmap_size_downsampled) { (cr, cc) =>

					// Find the index of the nonzero in this mxm (downsampled) grid
					val nonzero_idx_in_this_mxm = 0

					// Now we know where in the mxm our nonzero is, so get the upsampled
					// analogue for these downsampled coordinates
					val r_upsampled = cr*m + nonzero_idx_in_this_mxm/m
					val c_upsampled = cc*m + nonzero_idx_in_this_mxm%m

					// Finally, consider shifts
					val shifted_r_upsampled = r_upsampled - L1_fmap_offset_r
					val shifted_c_upsampled = c_upsampled - L1_fmap_offset_c

					// Have to shift because we need to convolve dJ/dL2 by moving it
					// around L1.t until we get a kxk kernel back
					if (shifted_c_upsampled < 0 || shifted_c_upsampled >= k_size ||
						shifted_r_upsampled < 0 || shifted_r_upsampled >= k_size) {
						0.0
					}
					else {
						val k_elem = w ((L1_fmap_num*k_total_size +
								(k_size-1-shifted_r_upsampled)*k_size + (k_size-1-shifted_c_upsampled)).toInt,
								L2_fmap_num)
						val dJ_dL2_elem = downsampled_dJ_dL2 (r,  // Current example
							  ( L2_fmap_num*fmap_total_size_downsampled + // Current L2 feature map
								// Get to the right mxm region for this L2 feature map
								cr*fmap_size_downsampled + 
								cc ).toInt
							)
						(dJ_dL2_elem * k_elem).toDouble
					}
				}
				conv.toDouble.sum
			}
		}
	}

	// Max pool for concatenated input feature maps
	// The input matrix X has 1 row per example. Each row contains "num_feature_maps"
	// concatenated feature maps, each of size "old_fmap_size" x "old_fmap_size"
	// (concatenated in row-major order). This function does mxm pooling to return
	// the same number of feature maps but smaller by a factor of mxm
	def max_pool_indices(X: Rep[DenseMatrix[Double]], m: Rep[Int], num_feature_maps: Rep[Int],
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		old_fmap_total_size: Rep[Int], // = X.numCols / num_feature_maps
		old_fmap_size: Rep[Int], // = sqrt(old_fmap_total_size.toDouble).toInt
		new_fmap_total_size: Rep[Int], // = old_fmap_total_size / (m*m)
		new_fmap_size: Rep[Int] // = old_fmap_size / m
	) = {

		val pooled = ( 0::X.numRows, 0::X.numCols/(m*m) ) { (r,c) =>
			val fmap_num = c / new_fmap_total_size // Old and new fmax number
			val mxm_box_row = (c%new_fmap_total_size) / new_fmap_size
			val mxm_box_col = (c%new_fmap_total_size) % new_fmap_size
			// This puts us at the top left corner of our mxm box in the source image
			// We are looking at each row of this mxm box in parallel below (iterator i)
			val index_of_mxm_feature_map = old_fmap_total_size*fmap_num + // Get to right example and feature map
							old_fmap_size*m*mxm_box_row + // Get to right row of source feature map for this mxm box
							m*mxm_box_col // Get to the col that is the top-left corner of this mxm box
			val view_of_this_example = X(r) // TODO: Verify this is not a copy
			val max_from_each_subrow = (0::m) { i => 
				val start_of_size_m_row_in_mxm = index_of_mxm_feature_map + 
					old_fmap_size*i // Within the mxm box, get to the row we're currently looking at
				// TODO: Ensure line below just takes a vview, not a copy
				view_of_this_example.slice(start_of_size_m_row_in_mxm, start_of_size_m_row_in_mxm+m).max
			}
			max_from_each_subrow.max
		}

		// TODO: This code is exactly the same as above but I return maxIndex instead of max
		// There should be some way to merge these?

		val indices = ( 0::X.numRows, 0::X.numCols/(m*m) ) { (r,c) =>
			val fmap_num = c / new_fmap_total_size // Old and new fmax number
			val mxm_box_row = (c%new_fmap_total_size) / new_fmap_size
			val mxm_box_col = (c%new_fmap_total_size) % new_fmap_size
			// This puts us at the top left corner of our mxm box in the source image
			// We are looking at each row of this mxm box in parallel below (iterator i)
			val index_of_mxm_feature_map = old_fmap_total_size*fmap_num + // Get to right example and feature map
							old_fmap_size*m*mxm_box_row + // Get to right row of source feature map for this mxm box
							m*mxm_box_col // Get to the col that is the top-left corner of this mxm box
			val view_of_this_example = X(r) // TODO: Verify this is not a copy
			val maxIdx_from_each_subrow = (0::m) { i => 
				val start_of_size_m_row_in_mxm = index_of_mxm_feature_map + 
					old_fmap_size*i // Within the mxm box, get to the row we're currently looking at
				// TODO: Ensure line below just takes a vview, not a copy
				view_of_this_example.slice(start_of_size_m_row_in_mxm, start_of_size_m_row_in_mxm+m).maxIndex
			}
			val max_from_each_subrow = (0::m) { i => 
				val start_of_size_m_row_in_mxm = index_of_mxm_feature_map + 
					old_fmap_size*i // Within the mxm box, get to the row we're currently looking at
				view_of_this_example(start_of_size_m_row_in_mxm + maxIdx_from_each_subrow(i))
			}
			max_from_each_subrow.maxIndex * m + maxIdx_from_each_subrow(max_from_each_subrow.maxIndex)
		}

		(pooled, indices)
	}


	// Helper function for converting a downsampled index and an upsample map 
	// to an upsampled index
	def get_upsampled_index_from_downsampled(dJ_do_DOWNSAMPLED: Rep[DenseMatrix[Double]], 
		layer1_num_hidden: Rep[Int], layer2_pool: Rep[Int], layer1_col: Rep[Int],
		upsample_idx: Rep[Int],
		// The remaining inputs can be determined from the above inputs, but the
		// code is faster if as many inputs as possible are constants
		fmap_total_size_downsampled: Rep[Int], // = dJ_do_DOWNSAMPLED.numCols / layer1_num_hidden
		fmap_size_downsampled: Rep[Int], // = sqrt(fmap_total_size_downsampled.toDouble).toInt

		fmap_total_size_upsampled: Rep[Int], // = fmap_total_size_downsampled * layer2_pool * layer2_pool
		fmap_size_upsampled: Rep[Int] // = sqrt(fmap_total_size_upsampled.toDouble).toInt
	) = {

		val fmap_num = layer1_col / fmap_total_size_downsampled
		val idx_of_upsampled_fmap = fmap_num*fmap_total_size_upsampled

		val downsampled_r = (layer1_col % fmap_total_size_downsampled) / fmap_size_downsampled
		val downsampled_c = (layer1_col % fmap_total_size_downsampled) % fmap_size_downsampled

		val upsampled_r = downsampled_r*layer2_pool + (upsample_idx / layer2_pool)
		val upsampled_c = downsampled_c*layer2_pool + (upsample_idx % layer2_pool)

		idx_of_upsampled_fmap + upsampled_r*fmap_size_upsampled + upsampled_c
	}

	// Shortcut to make a vector of references to matrices
	def v_of_m(size: Rep[Int]) = {
		(0::size) { t => DenseMatrix.zeros(1,1) }
	}

}

