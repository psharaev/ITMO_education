r1, s1, p1 = map(int, input().split())
r2, s2, p2 = map(int, input().split())
print(max(0, r1 - r2 - p2, p1 - p2 - s2, s1 - s2 - r2))
