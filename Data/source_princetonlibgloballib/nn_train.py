from tensorflow import keras
import tensorflow as tf
import numpy as np

def Model_Train(X, Y):
    N, m = np.shape(X)
    model = keras.Sequential([                            # Says that we are Sequentially modeling the data
        keras.layers.Flatten(input_shape=(m,)),       # Flattening/Unrolling the i/p of 28x28 Mx to an array of 256 attributes
        keras.layers.Dense(128, activation=tf.nn.relu),   # There are 128 neurons in the 1st Dense layer
        # keras.layers.Dense(128, activation=tf.nn.relu),   # There are 128 neurons in the 1st Dense layer
        keras.layers.Dense(1) #0, activation=tf.nn.softmax)  
    ])

    model.compile(optimizer='adam',
                  loss='logcosh',
                  metrics=['accuracy'])

    model.fit(X, Y, epochs=20)
    
    return model

def Grads_(model, X_): # X_ is one of the points in dataset to find the gradient at.
    outputTensor = model.output                  # Creating the output on which we try to find gradients
    # print('outputTensor',outputTensor)
    VariableTensors = model.trainable_weights[0] # Weights of first layer only so as to dy/dx and not the intermediate variables 'a' in between

    ipgrads = tf.keras.backend.gradients(outputTensor, VariableTensors)     
    # print('ipgrads',ipgrads)
    
    x_ = np.array([X_]) # X_ has a shape of (m,)
    # This gives x_ a shape of (1,m) which is permutable with the tensorflow 

    sess = tf.compat.v1.keras.backend.get_session()
    # print('sess', sess)
    Grad = sess.run(ipgrads, feed_dict={model.input:x_})
    grad = Grad[0]
    # print('grad',grad)
    
    Sum_grad = np.sum(grad,axis=1)
    return Sum_grad 
   
class Train():

    def __init__(self, X, Y, fn_x, x_sp = None, y_sp = None, Epochs = 1, n_iter = 1, max_iter = 30, max_epochs =5, Model = None, x_data = None):
        '''x_sp, y_sp: Starting Points 
        X, Y: Are data points on which to fit a model in Initializer and are single optimal till that iteration points when Recurring
        fn_x: Is the main function which returns black-box function's value when called '''
        self.Epochs = Epochs 
        self.X = X
        self.Y = Y
        self.fn_x = fn_x
        self.x_sp = x_sp
        self.y_sp = y_sp
        self.n_iter = n_iter
        self.max_iter = max_iter
        self.max_epochs = max_epochs
        self.Model = Model
        self.x_data = x_data

    def Initializer(self):
        
        print('Training model', self.Epochs)
        if self.y_sp == None:                          # i.e. 1st epoch 
            ind = np.argmin(self.Y)
            self.x_sp = np.array([self.X[ind]])        # x is an array of that point. Converted to a 2D array
            self.y_sp = np.array([self.Y[ind]])
            self.x_data = {}

        print('Starting point of x', self.x_sp)
        self.x_data[self.Epochs] = self.x_sp
        if self.Epochs == 1:
            print('###########################################################################')
            print(self.x_data)

        self.Model = Model_Train(self.X, self.Y)
            
        return self.Recursive_train()    # Recursive_train(Epochs, x_sp, y_sp, fn_x, Model, n_iter, max_iter, max_epochs)

# def Recursive_train(Epochs, x, y, fn_x, Model, n_iter, max_iter, max_epochs):
    def Recursive_train(self):
        '''x: Points collected till now
        y: Values upto this iteration
        Model: The model being used at this iteration'''
        # print('Started recursion')
        g = Grads_(self.Model, self.X[-1].flatten())

        # if self.Epochs > self.max_epochs:
        #     print('DONE iterations', self.Epochs)      
        #     print('x right now', self.X[-1])      

        #     return self.X, self.Y
        
        if self.n_iter > self.max_iter:
            print('Start training new model')
            '''New data points based on x values collected'''
            self.Epochs += 1
            print('# of Epochs', self.Epochs)
            if self.Epochs > self.max_epochs: 
                return self.X[-1], self.Y[-1]
            
            self.x_sp = np.array([self.X[-2]])
            self.y_sp = np.array([self.Y[-2]])
            X_data, Y_data = Create_data(x_Lower, x_Upper)
            # self.X = X_data
            # self.Y = Y_data
            self.X = np.append(X_data, self.x_sp, axis= 0)
            self.Y = np.append(Y_data, self.y_sp, axis= 0)
            
            self.n_iter = 0
            print('Starting point for next epoch', self.x_sp)

            # return Initializer(Epochs, X_data, Y_data, f, x_sp = np.array([x[-2]]), y_sp = np.array([y[-2]]), n_iter = 0, max_iter = 50, max_epochs=max_epochs)
            return self.Initializer()

        elif len(self.Y) <= 1 or self.Y[-1] + 1e-2 <= self.Y[-2]:
            # print('x[-1]', self.X[-1])
            # print('g', g)
            x_new = self.X[-1] - 0.001 * g                       # LR = 0.001
            x_new = x_new.reshape(1, len(x_new))

            if self.n_iter%10 == 0:
                print(self.n_iter, x_new, self.Y[-1])
            self.n_iter += 1
            y_new = self.fn_x(x_new)
            print('New y', y_new)

            self.Y = np.append(self.Y, y_new)
            self.X = np.append(self.X, x_new, axis = 0)

            return self.Recursive_train()
                
        elif self.Y[-1] + 1e-2 > self.Y[-2]:          #+ 1e-2
            print('Start training new model')
            '''New data points based on x values collected'''
            self.Epochs += 1
            print('# of Epochs', self.Epochs)
            if self.Epochs > self.max_epochs: 
                return self.X[-1], self.Y[-1]
            
            self.x_sp = np.array([self.X[-2]])
            self.y_sp = np.array([self.Y[-2]])
            X_data, Y_data = Create_data(x_Lower, x_Upper)
            # self.X = X_data
            # self.Y = Y_data
            self.X = np.append(X_data, self.x_sp, axis= 0)
            self.Y = np.append(Y_data, self.y_sp, axis= 0)
            
            self.n_iter = 0
            print('Starting point for next epoch', self.x_sp)

            # return Initializer(Epochs, X_data, Y_data, f, x_sp = np.array([x[-2]]), y_sp = np.array([y[-2]]), n_iter = 0, max_iter = 50, max_epochs=max_epochs)
            return self.Initializer()
    
def Create_data(x_Lower, x_Upper, X = {}, Y = {}, max_epochs = 10):
    N = 1000      # no. of samples
    m = 2      # no. of features

    X_data = np.random.randint(x_Lower, x_Upper, size = (N,m))    # Random pts created in specific region
    Y_data = f(X_data)
    
    return X_data, Y_data

####### Original commands #######

# N = 1000      # no. of samples
# m = 2      # no. of features
# x_Lower= 2
# x_Upper=6
# Epochs = 0

# def f(X):
#     # x1,x2= X
#     return np.sum(X**2, axis = 1)

# X0, Y0 = Create_data(x_Lower, x_Upper)
# # print(X0, Y0)
# Initializer(Epochs, X0, Y0, f)