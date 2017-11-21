package com.draper.cage.soot;

public interface CostVisitor
{
  public void visitIntegerCost(IntegerCost cost);
  
  public void visitMinCost(MinCost cost);
  
  public void visitMaxCost(MaxCost cost);
}
