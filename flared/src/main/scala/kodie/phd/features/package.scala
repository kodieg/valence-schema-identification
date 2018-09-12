package kodie.phd

/**
 * Created by kodie on 9/14/14.
 */
package object features {
  type Feature = String
  type FeatureExtractor[T] = (T => Feature)
}
