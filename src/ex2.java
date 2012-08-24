package edu.uchc.scheduler.Interface.fft;

import java.util.ArrayList;
import java.util.Hashtable;

import edu.uchc.scheduler.Interface.schedule.MultiDimensionQuadrature;
import edu.uchc.scheduler.Interface.schedule.MultiPhasePoint;
import edu.uchc.scheduler.Interface.schedule.Quadrature;


class FFT1DAlgorithm {


    //  @Deprecated
    //  static Complex[] computeFFT(ArrayList<SchedulePoint> points, int totpoints) {
    //      Complex[] x = new Complex[totpoints];
    //      
    //      //the existing values
    //      for(SchedulePoint p : points) {
    //          x[p.getXAsInt() - 1] = new Complex(1,1);
    //      }
    //      
    //      //the non-existing values
    //      for (int i = 0; i < totpoints; i++) {
    //          if (x[i] == null)
    //              x[i] = new Complex(0,0);
    //      }       
    //      
    //      // FFT of original data
    //      x = zerofill(x);
    //      fft(x);
    //      rotate(x);
    //      return x;
    //  }


    static Complex[] computeFFTWithQuadrature(ArrayList<MultiPhasePoint> points, int totpoints) {
        Complex[] x = new Complex[totpoints];

        ComplexPoint[] cPoints = convertPoints(points);
        //the existing values
        for(ComplexPoint p : cPoints) {
            x[p.x - 1] = new Complex(p.realValue(), p.imaginaryValue());
        }

        //the non-existing values
        for (int i = 0; i < totpoints; i++) {
            if (x[i] == null) {
                x[i] = new Complex(0,0);
            }
        }       

        // FFT of original data
        x = zerofill(x);
        fft(x);
        rotate(x);
        return x;
    }

    static ComplexPoint[] convertPoints(ArrayList<MultiPhasePoint> points) {
        ArrayList<ComplexPoint> cPoints = new ArrayList<ComplexPoint>();
        for(MultiPhasePoint s : points) {
            ComplexPoint cPoint = new ComplexPoint(s.getXAsInt());
            for(MultiDimensionQuadrature q : s.getQuadratures()) {
                cPoint.addQuadrature(q.getXQuadrature());
            }
            cPoints.add(cPoint);
        }
        return cPoints.toArray(new ComplexPoint [cPoints.size()]);
    }


    // compute the FFT of x[], assuming its length is a power of 2
    static void fft(Complex[] x) {

        // check that length is a power of 2
        int N = x.length;
        if (Integer.highestOneBit(N) != N) {
            throw new RuntimeException("N is not a power of 2 "+x.length);
        }

        // bit reversal permutation
        int shift = 1 + Integer.numberOfLeadingZeros(N);
        for (int k = 0; k < N; k++) {
            int j = Integer.reverse(k) >>> shift;
        if (j > k) {
            Complex temp = x[j];
            x[j] = x[k];
            x[k] = temp;
        }
        }

        // butterfly updates
        for (int L = 2; L <= N; L = L+L) {
            for (int k = 0; k < L/2; k++) {
                double kth = -2 * k * Math.PI / L;
                Complex w = new Complex(Math.cos(kth), Math.sin(kth));
                for (int j = 0; j < N/L; j++) {
                    Complex tao = null;
                    try {
                        tao = w.times(x[j*L + k + L/2]);
                    } catch (Exception e ) {System.out.println("x index "+j*L + k + L/2);}
                    x[j*L + k + L/2] = x[j*L + k].minus(tao); 
                    x[j*L + k]       = x[j*L + k].plus(tao); 
                }
            }
        }
    }



    static Complex[] zerofill(Complex[] x)  {
        int fn = getNearestFourierNumber(x.length);
        if(x.length==fn) return x;          //already Fourier
        Complex[] y = new Complex[fn];

        for(int i = 0; i<x.length; i++) {
            y[i]=x[i];                  //keep the old values
        }

        for(int i = x.length-1; i<fn; i++)  {
            y[i] = new Complex(0,0);        //zerofill
        }

        return y;

    }

    static int getNearestFourierNumber(int n)   {
        if (n<=0) return 1;
        else if (n==1) return 1;

        int c = 2;
        while (c < n)   {
            c = c*2;
        }
        //System.out.println("Value " + n + " changed to Fourier number " + c);
        return c;
    }

    static Complex[] rotate(Complex[] x) {
        Complex a;
        int length = x.length;
        for(int i = 0; i<length/2; i++) {
            a = x[i];
            x[i] = x[length/2+i];
            x[length/2+i] = a;
        }
        return x;
    }   

    static class ComplexPoint {

        final int x;
        Hashtable<String, Boolean> quadrature = new Hashtable<String, Boolean>(2);

        ComplexPoint(int x) {
            this.x = x;
            this.quadrature.put(Quadrature.REAL.asString(), false);
            this.quadrature.put(Quadrature.IMAGINARY.asString(), false);
        }

        void addQuadrature(Quadrature q) {
            this.quadrature.put(q.asString(), true);
        }

        int realValue() {
            if(this.quadrature.get(Quadrature.REAL.asString())) {
                return 1;
            }
            return 0;
        }

        int imaginaryValue() {
            if(this.quadrature.get(Quadrature.IMAGINARY.asString())) {
                return 1;
            }
            return 0;
        }
    }
}
