//  
//  Stanford PPL
//  http://ppl.stanford.edu/
//

import optiml.compiler._
import optiml.library._
import optiml.shared._

trait NetLib extends OptiMLApplication {

  // Calculate and return cross entropy
  // Objective functions usually try to maximize this
  // (Or more commonly, to minimize J = -CE + weight decay)
  def cross_entropy(softmax_out: Rep[DenseMatrix[Float]], y: Rep[DenseVector[Float]]) = {

    val CE_Matrix = (0::softmax_out.numRows, 0::softmax_out.numCols) { (i,j) =>
      val prob = softmax_out(i,j)
      if (y(i) == j) log(prob).toFloat else 0.0f
    }

    // TODO: Time against this:
    //val CE_Vector = (0::softmax_out.numRows) { i => log(softmax_out(i,y(i))).toFloat }

    CE_Matrix.sum
  }

  // Activation functions
  def logistic(z: Rep[Float]) : Rep[Float] = {
    1.0f / (1.0f + exp(z * -1.0f).toFloat)
  }
  def ReLU(z: Rep[Float]) : Rep[Float] = {
    if (z <= 0) 0.0f
    else z
  }
  def ReLU_Deriv(z: Rep[Float]) : Rep[Float] = {
    if (z <= 0) 0.0f else 1.0f
  }

  // Forward through fully connected layer
  def fullycon_fw(X: Rep[DenseMatrix[Float]], W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]]) = {
    (0::X.numRows, 0::W.numCols) { (r,c) =>
      val s = sum(0, X.numCols) { i =>
        X(r,i)*W(i,c)
      }
      s + b(c)
    }
  }

  // Forward through fully connected layer
  // For recurrent neural networks
  def fullycon_fw_RNN(X: Rep[DenseMatrix[Float]], H: Rep[DenseMatrix[Float]], Wx: Rep[DenseMatrix[Float]], 
    b: Rep[DenseVector[Float]], Wh: Rep[DenseMatrix[Float]]) = {

    // TODO: Make a non-BLAS version of this function
    // Note: X.numRows = H.numRows

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
  def fullycon_fw_blas(X: Rep[DenseMatrix[Float]], W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]]) = {

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
  def softmax_fw(X: Rep[DenseMatrix[Float]], W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]]) = {
    val prod = (0::X.numRows, 0::W.numCols) { (r,c) =>
      val s = sum(0, X.numCols) { i =>
        X(r,i)*W(i,c)
      }
      s + b(c)
    }
    val Py_given_X = ( prod ).map(e => exp(e).toFloat) // exp(_)
    val sum_of_rows = (0::X.numRows) {
      r => Py_given_X.getRow(r).sum
    } 
    (0::X.numRows, 0::W.numCols) { (i,j) =>
      Py_given_X(i,j)/sum_of_rows(i)
    }
  }

  // Forward through softmax
  // BLAS version
  def softmax_fw_blas(X: Rep[DenseMatrix[Float]], W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]]) = {

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
    // TODO: The loop below cannot be fused if we used a blas call, but
    // we can get rid of this loop by adding an extra column of 1's to the
    // data matrix and the biases as the final row of the weights.
    val prod = (0::prod_no_bias.numRows, 0::prod_no_bias.numCols) { (r,c) =>
      prod_no_bias(r,c) + b(c)
    }
    val Py_given_X = ( prod ).map(e => exp(e).toFloat) // exp(_)
    val sum_of_rows = (0::X.numRows) {
      r => Py_given_X.getRow(r).sum
    } 
    (0::X.numRows, 0::W.numCols) {
      (i,j) => Py_given_X(i,j)/sum_of_rows(i)
    }
  }

  def get_classification_error(all_predictions: Rep[DenseMatrix[Float]], y: Rep[DenseVector[Float]]) = {

    val num_test = y.length

    // Following the softmax, last step is to select best value from each category
    val predictions = (0::num_test) { r => all_predictions.getRow(r).maxIndex }

    // Compare to the actual classes and print result
    val results = (0::predictions.length) { i =>
      if (predictions(i) == y(i)) 0 else 1
    }
    results.sum.toFloat / num_test.toFloat * 100
  }

  def print_classification_error(all_predictions: Rep[DenseMatrix[Float]], y: Rep[DenseVector[Float]]) = {

    val classification_error = get_classification_error(all_predictions, y)
    print("Classification Error (")
    print(y.length)
    print(" examples):  ")
    print(classification_error)
    println(" %")
  }

  // Input:  A matrix where each row contains num_feature_maps feature maps concatenated
  //         in row-major order
  // Output: A matrix of matrices (feature maps)
  //
  // This function is currently unused, but would be used for nested data-structures with
  // nested parallelism.
  def reshape_2D_Tensor_to_4D_Tensor(T_2D: Rep[DenseMatrix[Float]], num_feature_maps: Rep[Int],
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    feature_map_size: Rep[Int],  // = T_2D.numCols / num_feature_maps
    matrix_side_length: Rep[Int] // = sqrt(feature_map_size.toFloat).toInt

  ) = {
    (0::T_2D.numRows, 0::num_feature_maps) { (r,c) =>
      (0::matrix_side_length, 0::matrix_side_length) {
        (i,j) => T_2D(r, c*feature_map_size + i*matrix_side_length + j)
      }
    }
  }

  // Input:  A matrix of matrices (feature maps)
  // Output: A matrix where each row contains num_feature_maps feature maps concatenated
  //         in row-major order
  //
  // This function is currently unused, but would be used for nested data-structures with
  // nested parallelism.
  def reshape_4D_Tensor_to_2D_Tensor(T_4D: Rep[DenseMatrix[DenseMatrix[Float]]],
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

  // Forward propagation through convolutional layer
  //
  // NOTE: This uses zero-padding, so no information is thrown away
  // and the feature map size is unchanged by convolution.
  // This can instead be specified as an option (how much to zero-pad), which may
  // provide some speedups because:
  // - Less computation
  // - Convolutions further reduce feature map sizes
  // - Easier BackProp (ignore borders)
  // - Prevents divergence on GPU (esp. for small fmaps where borders dominate)
  // - Doesn't get much worse in terms of accuracy
  def conv_fw(X: Rep[DenseMatrix[Float]], num_feature_maps_L1: Rep[Int],
    W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]], 
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    num_feature_maps_L2: Rep[Int], // = W.numCols
    num_2D_kernel_elements: Rep[Int], // = W.numRows / num_feature_maps_L1 // Square
    kernel_size: Rep[Int], // = sqrt(num_2D_kernel_elements.toFloat).toInt
    half_convolution_length: Rep[Int], // = (kernel_size)/2

    feature_map_total_size: Rep[Int], // = X.numCols / num_feature_maps_L1
    feature_map_size: Rep[Int] // = sqrt(feature_map_total_size.toFloat).toInt
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
              0.0f
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

  // Forward propagation through convolutional layer
  //
  // An alternate implementation of the fw prop convolution which uses a large
  // blas call, as in cuDNN. 
  // TODO: Finish implementing this
  // TODO: Do the same thing for the backwards convolutions, which have special
  // cases due to merging upsampling and convolution (which gives speedups)

  // pure version of matrix lowering
  def matrix_lowering(X: Rep[DenseMatrix[Float]], row: Rep[Int], kernel_size: Rep[Int],
          channels: Rep[Int], width: Rep[Int], height: Rep[Int],
          width_col: Rep[Int], height_col: Rep[Int], kernel_h: Rep[Int], kernel_w: Rep[Int], 
          stride_h: Rep[Int], stride_w: Rep[Int], pad_h: Rep[Int], pad_w: Rep[Int]) = {
    (0::channels*kernel_h*kernel_w,0::width_col*height_col) { (r,c) =>
        val ch = r / (kernel_h*kernel_w)
        val kh = (r % (kernel_h*kernel_w)) / kernel_w
        val kw = (r % (kernel_h*kernel_w)) % kernel_w
        val oh = c / width_col
        val ow = c % width_col
        val ih = oh * stride_h - pad_h
        val iw = ow * stride_w - pad_w
        if (ih+kh < height && iw+kw < width) 
          X(row, ch*height*width + (ih+kh)*width + (iw+kw))
        else 
          0.0f 
    }   
  }

  // mutable version of matrix lowering
  def matrix_lowering_m(X: Rep[DenseMatrix[Float]], row: Rep[Int], kernel_size: Rep[Int],
          channels: Rep[Int], width: Rep[Int], height: Rep[Int],
          width_col: Rep[Int], height_col: Rep[Int], kernel_h: Rep[Int], kernel_w: Rep[Int], 
          stride_h: Rep[Int], stride_w: Rep[Int], pad_h: Rep[Int], pad_w: Rep[Int], out: Rep[DenseMatrix[Float]]) = {
    for (index <- 0::channels*width_col*height_col) {
        val w_out = index % width_col
        val h_index = index / width_col
        val h_out = h_index % height_col
        val channel_in = h_index / height_col
        val channel_out = channel_in * kernel_h * kernel_w
        val h_in = h_out * stride_h - pad_h
        val w_in = w_out * stride_w - pad_w
        var out_offset_h = channel_out
        val in_offset = (channel_in * height + h_in) * width + w_in
        var i = 0
        while (i < kernel_h) {
            var j = 0
            while (j < kernel_w) {
                val h = h_in + i
                val w = w_in + j
                out(out_offset_h,h_out*width_col+w_out) = if (h >= 0 && w >= 0 && h < height && w < width) X(row,in_offset + i * width + j) else 0.0f
                out_offset_h += 1
                j += 1
            }
            i += 1
        }
    }   
  }

  // convolution layer that uses mutable data structure for lowering
  def conv_fw_gemm_m(X: Rep[DenseMatrix[Float]], num_feature_maps_L1: Rep[Int],
    W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]], 
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    num_feature_maps_L2: Rep[Int], // = W.numCols
    num_2D_kernel_elements: Rep[Int], // = W.numRows / num_feature_maps_L1 // Square
    kernel_size: Rep[Int], // = sqrt(num_2D_kernel_elements.toFloat).toInt
    half_convolution_length: Rep[Int], // = (kernel_size)/2
    feature_map_total_size: Rep[Int], // = X.numCols / num_feature_maps_L1
    feature_map_size: Rep[Int], // = sqrt(feature_map_total_size.toFloat).toInt
    stride: Rep[Int], height: Rep[Int], width: Rep[Int]) = {
      val m = DenseMatrix.zerosf(X.numRows,feature_map_total_size).mutable
      val lm = DenseMatrix.zerosf(num_2D_kernel_elements*num_feature_maps_L1,feature_map_total_size).mutable
      var i = 0
      while (i < X.numRows) {
          matrix_lowering_m(X,i,kernel_size,num_feature_maps_L1,height,width,feature_map_size,feature_map_size,kernel_size,kernel_size,stride,stride,0,0,lm)
          val o = W * lm
          for (j <- 0::(num_feature_maps_L2 * feature_map_total_size)) {
                  m(i,j) = o(j/feature_map_total_size,j%feature_map_total_size) 
          }
          i += 1
      }
      m
  }

  // convolution layer that uses pure version of lowering
  def conv_fw_gemm(X: Rep[DenseMatrix[Float]], num_feature_maps_L1: Rep[Int],
    W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]], 
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    num_feature_maps_L2: Rep[Int], // = W.numCols
    num_2D_kernel_elements: Rep[Int], // = W.numRows / num_feature_maps_L1 // Square
    kernel_size: Rep[Int], // = sqrt(num_2D_kernel_elements.toFloat).toInt
    half_convolution_length: Rep[Int], // = (kernel_size)/2
    feature_map_total_size: Rep[Int], // = X.numCols / num_feature_maps_L1
    feature_map_size: Rep[Int], // = sqrt(feature_map_total_size.toFloat).toInt
    stride: Rep[Int], height: Rep[Int], width: Rep[Int]) = {
      val m = DenseMatrix.zerosf(X.numRows,feature_map_total_size).mutable
      var i = 0
      while (i < X.numRows) {
          val lm = matrix_lowering(X,i,kernel_size,num_feature_maps_L1,height,width,feature_map_size,feature_map_size,kernel_size,kernel_size,stride,stride,0,0)
          val o = W * lm
          for (j <- 0::(num_feature_maps_L2 * feature_map_total_size)) {
                  m(i,j) = o(j/feature_map_total_size,j%feature_map_total_size) 
          }
          i += 1
      }
      m
  }

  // convolution layer that is pure and parallelizes over batches
  def conv_fw_gemm_parallel(X: Rep[DenseMatrix[Float]], num_feature_maps_L1: Rep[Int],
    W: Rep[DenseMatrix[Float]], b: Rep[DenseVector[Float]], 
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    num_feature_maps_L2: Rep[Int], // = W.numCols
    num_2D_kernel_elements: Rep[Int], // = W.numRows / num_feature_maps_L1 // Square
    kernel_size: Rep[Int], // = sqrt(num_2D_kernel_elements.toFloat).toInt
    half_convolution_length: Rep[Int], // = (kernel_size)/2
    feature_map_total_size: Rep[Int], // = X.numCols / num_feature_maps_L1
    feature_map_size: Rep[Int], // = sqrt(feature_map_total_size.toFloat).toInt
    stride: Rep[Int], height: Rep[Int], width: Rep[Int]) = {
      (0::X.numRows, *) { i => 
          val lm = matrix_lowering(X,i,kernel_size,num_feature_maps_L1,height,width,feature_map_size,feature_map_size,kernel_size,kernel_size,stride,stride,0,0)
          val o = W * lm
          (0::num_feature_maps_L2*feature_map_total_size) { j => o(j/feature_map_total_size,j%feature_map_total_size) }
      }
  }

  // Back propagation through convolutional layer (with respect to weights)
  //
  // Returns gradient of cost function wrt the convolutional weights of this
  // convolutional layer
  //
  // Note: This assumes the convolutional layer was followed by a max pool
  // layer and combines the backwards pooling (upsampling) with the convolution.
  // A more modular approach would have been to have a function which does the
  // backward pass over the pool layer, then pass the upsampled layer to this
  // function to go backwards through the convolutional layer, but it is ~2x faster
  // to skip the upsampling step and directly upsample/convolve (even for the smallest
  // pooling of 2x2) because in the 2x2 case there are 4x fewer computations needed,
  // since following backprop through max-pool layers (upsampling) 1/(m^2) elements
  // are 0, where m is the pooling factor.
  def conv_bw_weights
  (
    k_total_size: Rep[Int], L1_num_fmaps: Rep[Int], L2_num_fmaps: Rep[Int],
    L1: Rep[DenseMatrix[Float]], downsampled_dJ_dL2: Rep[DenseMatrix[Float]],
    m: Rep[Int], L2_upsample_map: Rep[DenseMatrix[Int]],  // "m" is pooling size
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    k_size: Rep[Int], // = sqrt(k_total_size.toFloat).toInt
    half_k_size: Rep[Int], // = (k_size)/2

    fmap_total_size: Rep[Int], // = L1.numCols / L1_num_fmaps
    fmap_size: Rep[Int], // = sqrt(fmap_total_size.toFloat).toInt
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
            0.0f
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
            (dJ_dL2_elem * L1_elem).toFloat
          }
        }
        conv.toFloat.sum
      }
    }
  }

  // Back propagation through convolutional layer (with respect to weights)
  //
  // Same as conv_bw_weights but without pooling, i.e. conv_bw_weights takes
  // pooling indices (for upsampling) as well as a pool factor, assuming 
  // that the convolution layer was followed by a max pool layer. If that's
  // not the case, then we could have just passed a pool factor of 1 and an
  // upsample matrix of 0 for all indices. That is what that function does,
  // but without the extra arguments and matrix reads.
  def conv_bw_weights_no_pool
  (
    k_total_size: Rep[Int], L1_num_fmaps: Rep[Int], L2_num_fmaps: Rep[Int],
    L1: Rep[DenseMatrix[Float]], downsampled_dJ_dL2: Rep[DenseMatrix[Float]],

    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    k_size: Rep[Int], // = sqrt(k_total_size.toFloat).toInt
    half_k_size: Rep[Int], // = (k_size)/2

    fmap_total_size: Rep[Int], // = L1.numCols / L1_num_fmaps
    fmap_size: Rep[Int], // = sqrt(fmap_total_size.toFloat).toInt
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
            0.0f
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
            (dJ_dL2_elem * L1_elem).toFloat
          }
        }
        conv.toFloat.sum
      }
    }
  }

  // TODO: All these fw prop and bp methods just do convolution,
  // should combine the functionality to just call a "conv" method


  // Back propagation through convolutional layer (with respect to data)
  //
  // Returns gradient of cost function wrt the input feature maps of this
  // convolutional layer
  //
  // Note: This assumes the convolutional layer was followed by a max pool
  // layer and combines the backwards pooling (upsampling) with the convolution.
  // A more modular approach would have been to have a function which does the
  // backward pass over the pool layer, then pass the upsampled layer to this
  // function to go backwards through the convolutional layer, but it is ~2x faster
  // to skip the upsampling step and directly upsample/convolve (even for the smallest
  // pooling of 2x2) because in the 2x2 case there are 4x fewer computations needed,
  // since following backprop through max-pool layers (upsampling) 1/(m^2) elements
  // are 0, where m is the pooling factor.
  def conv_bw_data
  (
    w: Rep[DenseMatrix[Float]], L1_num_fmaps: Rep[Int],
    downsampled_dJ_dL2: Rep[DenseMatrix[Float]],
    m: Rep[Int], L2_upsample_map: Rep[DenseMatrix[Int]], // "m" is pooling size
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    L2_num_fmaps: Rep[Int], // = w.numCols
    k_total_size: Rep[Int], // = w.numRows / L1_num_fmaps
    k_size: Rep[Int], // = sqrt(k_total_size.toFloat).toInt

    fmap_total_size_downsampled: Rep[Int], // = downsampled_dJ_dL2.numCols / L2_num_fmaps
    fmap_size_downsampled: Rep[Int], // = sqrt(fmap_total_size_downsampled.toFloat).toInt

    fmap_total_size_upsampled: Rep[Int], // = fmap_size_upsampled*fmap_size_upsampled
    fmap_size_upsampled: Rep[Int], // = fmap_size_downsampled*m
    half_L1_fmap_size: Rep[Int], // = (fmap_size_upsampled.toFloat)/2.0

    centering_offset: Rep[Float] // = ((k_size - fmap_size_upsampled).toFloat)/2.0
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
            0.0f
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
            (dJ_dL2_elem * k_elem).toFloat
          }
        }
        conv.toFloat.sum
      }
    }
  }

  // Back propagation through convolutional layer (with respect to data)
  //
  // Same as conv_bw_data but without pooling, i.e. conv_bw_data takes
  // pooling indices (for upsampling) as well as a pool factor, assuming 
  // that the convolution layer was followed by a max pool layer. If that's
  // not the case, then we could have just passed a pool factor of 1 and an
  // upsample matrix of 0 for all indices. That is what that function does,
  // but without the extra arguments and matrix reads.
  def conv_bw_data_no_pool
  (
    w: Rep[DenseMatrix[Float]], L1_num_fmaps: Rep[Int],
    downsampled_dJ_dL2: Rep[DenseMatrix[Float]],

    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    L2_num_fmaps: Rep[Int], // = w.numCols
    k_total_size: Rep[Int], // = w.numRows / L1_num_fmaps
    k_size: Rep[Int], // = sqrt(k_total_size.toFloat).toInt

    fmap_total_size_downsampled: Rep[Int], // = downsampled_dJ_dL2.numCols / L2_num_fmaps
    fmap_size_downsampled: Rep[Int], // = sqrt(fmap_total_size_downsampled.toFloat).toInt

    fmap_total_size_upsampled: Rep[Int], // = fmap_size_upsampled*fmap_size_upsampled
    fmap_size_upsampled: Rep[Int], // = fmap_size_downsampled*m
    half_L1_fmap_size: Rep[Int], // = (fmap_size_upsampled.toFloat)/2.0

    centering_offset: Rep[Float] // = ((k_size - fmap_size_upsampled).toFloat)/2.0
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
            0.0f
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
            (dJ_dL2_elem * k_elem).toFloat
          }
        }
        conv.toFloat.sum
      }
    }
  }

  // Max pool for concatenated input feature maps
  // The input matrix X has 1 row per example. Each row contains "num_feature_maps"
  // concatenated feature maps, each of size "old_fmap_size" x "old_fmap_size"
  // (concatenated in row-major order). This function does mxm pooling to return
  // the same number of feature maps but smaller by a factor of mxm
  def max_pool_indices(X: Rep[DenseMatrix[Float]], m: Rep[Int], num_feature_maps: Rep[Int],
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    old_fmap_total_size: Rep[Int], // = X.numCols / num_feature_maps
    old_fmap_size: Rep[Int], // = sqrt(old_fmap_total_size.toFloat).toInt
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
      //val view_of_this_example = X(r) // TODO: Verify this is not a copy
      var i = 0
      var maxVal = -10000.0f
      while (i < m) {
          var j = 0
          while (j < m) {
              val v = X(r,index_of_mxm_feature_map + old_fmap_size*readVar(i) + readVar(j))
              if (v > maxVal) maxVal = v
              j += 1
          }
          i += 1
      }
      maxVal
          /*
      val max_from_each_subrow = (0::m) { i => 
        val start_of_size_m_row_in_mxm = index_of_mxm_feature_map + 
          old_fmap_size*i // Within the mxm box, get to the row we're currently looking at
        // TODO: Ensure line below just takes a vview, not a copy
        view_of_this_example.slice(start_of_size_m_row_in_mxm, start_of_size_m_row_in_mxm+m).max
      }
      max_from_each_subrow.max
      */
    }

    // TODO: This code is exactly the same as above but I return maxIndex instead of max
    // There should be some way to fuse these?

    val indices = ( 0::X.numRows, 0::X.numCols/(m*m) ) { (r,c) =>
      val fmap_num = c / new_fmap_total_size // Old and new fmax number
      val mxm_box_row = (c%new_fmap_total_size) / new_fmap_size
      val mxm_box_col = (c%new_fmap_total_size) % new_fmap_size
      // This puts us at the top left corner of our mxm box in the source image
      // We are looking at each row of this mxm box in parallel below (iterator i)
      val index_of_mxm_feature_map = old_fmap_total_size*fmap_num + // Get to right example and feature map
              old_fmap_size*m*mxm_box_row + // Get to right row of source feature map for this mxm box
              m*mxm_box_col // Get to the col that is the top-left corner of this mxm box
      var i = 0
      var maxVal = -1000000.0f
      var maxIdx = 0
      while (i < m) {
          var j = 0
          while (j < m) {
              val v = X(r,index_of_mxm_feature_map + old_fmap_size*readVar(i) + readVar(j))
              if (v > maxVal) { maxVal = v; maxIdx = i*m+j; }
              j += 1
          }
          i += 1
      }
      maxIdx
          /*
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
      */
    }

    (pooled, indices)
  }


  // Helper function for converting a downsampled index and an upsample map 
  // to an upsampled index
  def get_upsampled_index_from_downsampled(dJ_do_DOWNSAMPLED: Rep[DenseMatrix[Float]], 
    layer1_num_hidden: Rep[Int], layer2_pool: Rep[Int], layer1_col: Rep[Int],
    upsample_idx: Rep[Int],
    // The remaining inputs can be determined from the above inputs, but the
    // code is faster if as many inputs as possible are constants
    fmap_total_size_downsampled: Rep[Int], // = dJ_do_DOWNSAMPLED.numCols / layer1_num_hidden
    fmap_size_downsampled: Rep[Int], // = sqrt(fmap_total_size_downsampled.toFloat).toInt

    fmap_total_size_upsampled: Rep[Int], // = fmap_total_size_downsampled * layer2_pool * layer2_pool
    fmap_size_upsampled: Rep[Int] // = sqrt(fmap_total_size_upsampled.toFloat).toInt
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
    (0::size) { t => DenseMatrix.zerosf(1,1) }
  }

}

