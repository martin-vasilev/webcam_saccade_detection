import hashlib
import numpy as np

def digest_ids(x):return hashlib.sha256(','.join(map(str,sorted(map(int,x)))).encode()).hexdigest()

def mcc(a):
 a=np.asarray(a,dtype=float);t,n,f,g=np.moveaxis(a,-1,0)
 den=np.sqrt((t+f)*(t+g)*(n+f)*(n+g))
 return np.divide(t*n-f*g,den,out=np.full(np.shape(den),np.nan),where=den>0)

def counts(y,p):return np.array([sum(y&p),sum(~y&~p),sum(~y&p),sum(y&~p)],dtype=np.int64)
