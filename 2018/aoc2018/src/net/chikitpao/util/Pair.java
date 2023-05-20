// Pair.java
// AoC 2018
// Author: Chi-Kit Pao
//


package net.chikitpao.util;

public class Pair<T, Q> {
    public T first;
    public Q second;
    public Pair(T first, Q second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(Object obj) {
        if(obj == null)
            return false;
        if(!(obj instanceof Pair))
            return false;
        return first.equals(((Pair) obj).first) && second.equals(((Pair) obj).second);
    }
    @Override
    public int hashCode(){
        // Must the factor be prime?
        return first.hashCode() * 1021 + second.hashCode();
    }
}
