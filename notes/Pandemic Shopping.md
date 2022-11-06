---
name: Pandemic Shopping
url: https://open.kattis.com/problems/pandemicshopping
---

# Pandemic Shopping

Naive brute force method takes up to $2^{32}$, not going to work

* The top and bottom aisles are fixed based on the directions of the north/south aisles or the shoppers will get stuck in a corner or not be able to reach the top or bottom product aisles
* If both vertical aisles A and B point in the same direction, there is only one possible path through because it must zig-zag back and forth through the aisles
* If the vertical aisles point in different directions, the interior aisles can go either way (the top/bottom aisles are still determined and in fact the perimeter is necessarily a cycle) because the shopper can always loop around if needed. So each unspecified access aisle can be either direction, making this a simple 2^x calculation after verifying the specified aisles form a so-far-valid floor plan
