## turn the images 'npy/subs.npy' into variograms 'npy/mvgs.npy'

import tensorflow as tf
import numpy as np
import os
from sklearn.preprocessing import StandardScaler

class ImageVariogramGenerator(tf.keras.utils.Sequence):
    def __init__(self, verbose = True):
        self.verbose = verbose

    def squared_diff(self, i, im):
        # self.x: input data with shape NHWC
        # i: int index to go out
        dist = np.sqrt(i**2 + np.arange(0,i+1)**2)
        if i == 1:
            ix = [[i, 0], [i, -i]]
            iy = [[0, i], [i,  i]]
        else:
            ix = [[i, k] for k in range(1,i)]
            ix.insert(0, [i, 0])
            ix.append([i, -i])
            iy = [[k, i] for k in range(1,i)]
            iy.insert(0, [0, i])
            iy.append([i, i])
        diff = []
        w = np.zeros(len(dist))
        for j in range(len(dist)):
            w[j] = (im.shape[1]-ix[j][0]) * (im.shape[2]-iy[j][0]) * 2
            if j == 0:
                tmp = [tf.expand_dims(
                    tf.reduce_sum(tf.math.squared_difference(im[:, ix[j][k]:, iy[j][k]:, :],
                                                             im[:, 0:(im.shape[1]-ix[j][k]), 0:(im.shape[2]-iy[j][k]), :]),
                                  axis=[1,2]), axis=2)
                       for k in range(2)]
                tmp = tf.expand_dims(tf.reduce_sum(tf.concat(tmp, axis=2), axis=2), axis=2)
            elif j == len(dist)-1:
                tmp1 = tf.expand_dims(
                    tf.reduce_sum(tf.math.squared_difference(im[:, ix[j][0]:, iy[j][0]:, :],
                                                             im[:, 0:(im.shape[1]-ix[j][0]), 0:(im.shape[2]-iy[j][0]), :]),
                                  axis=[1,2]), axis=2)
                tmp2 = tf.expand_dims(
                    tf.reduce_sum(tf.math.squared_difference(im[:, 0:(im.shape[1]-ix[j][0]), iy[j][0]:, :],
                                                             im[:, ix[j][0]:, 0:(im.shape[2]-iy[j][0]), :]),
                                  axis=[1,2]), axis=2)
                tmp = tf.expand_dims(tf.reduce_sum(tf.concat([tmp1, tmp2], axis=2), axis=2), axis=2)
            else:
                w[j] *= 2
                tmp1 = [tf.expand_dims(
                    tf.reduce_sum(tf.math.squared_difference(im[:, ix[j][k]:, iy[j][k]:, :],
                                                             im[:, 0:(im.shape[1]-ix[j][k]), 0:(im.shape[2]-iy[j][k]), :]),
                                  axis=[1,2]), axis=2)
                        for k in range(2)]
                tmp1 = tf.expand_dims(tf.reduce_sum(tf.concat(tmp1, axis=2), axis=2), axis=2)
                tmp2 = [tf.expand_dims( # x shifted in other direction ..
                    tf.reduce_sum(tf.math.squared_difference(im[:, 0:(im.shape[1]-ix[j][k]), iy[j][k]:, :],
                                                             im[:, ix[j][k]:, 0:(im.shape[2]-iy[j][k]), :]),
                                  axis=[1,2]), axis=2)
                        for k in range(2)]
                tmp2 = tf.expand_dims(tf.reduce_sum(tf.concat(tmp2, axis=2), axis=2), axis=2)
                tmp = tf.expand_dims(tf.reduce_sum(tf.concat([tmp1, tmp2], axis=2), axis=2), axis=2)
            diff.append(tmp / w[j])
        diff = tf.concat(diff, axis=2)
        return diff, dist, w

    def vgram(self, im):
        diff, dist, w = self.squared_diff(i=1, im=im)
        for i in range(2, im.shape[1]): # probably tf.map_fn would be faster
            tmp_diff, tmp_dist, tmp_w = self.squared_diff(i=i, im=im)
            diff = tf.concat([diff, tmp_diff], axis=2)
            dist = np.concatenate([dist, tmp_dist])
            w    = np.concatenate([w, tmp_w])
        # sort according to dist
        ind  = tf.argsort(dist)
        diff = tf.gather(diff, ind, axis=2)
        dist = tf.gather(dist, ind)
        w    = tf.gather(w,    ind)
        # combine results with same dist
        diff = tf.cast(diff, dtype=tf.float32)
        w = tf.cast(w, dtype=tf.float32)
        uni = tf.unique(dist)[1]
        dist = tf.math.segment_sum(dist, uni)
        tmp1 = tf.transpose(diff * tf.tile(tf.reshape(w, [1,1,len(w)]),
                                           [diff.shape[0],diff.shape[1],1]), [2,0,1])
        diff = tf.transpose(tf.math.segment_sum(tmp1, uni), [1,2,0])
        w = tf.math.segment_sum(w, uni)
        diff /= tf.tile(tf.reshape(w, [1,1,len(w)]), [diff.shape[0],diff.shape[1], 1])
                        
        vg = tf.transpose(tf.expand_dims(diff, 3), [0,2,3,1]) / 2
        return vg, dist, w

    def multivariofn(self, ims):
        if self.verbose:
            tf.print('.', end='')
        vgs, _, _ = self.vgram(tf.expand_dims(ims[:,:,:,0], 3))
        if ims.shape[3] > 1:
            for i in range(1, ims.shape[3]):
                vg, _, _ = self.vgram(tf.expand_dims(ims[:,:,:,i], 3))
                vgs = tf.concat([vgs, vg], axis=3)
                if self.verbose:
                    tf.print('.', end='')
        return vgs

ims=np.load('npy/subs.npy')
generator=ImageVariogramGenerator()
mvgs=generator.multivariofn(ims=ims)
np.save('npy/mvgs.npy', mvgs)
