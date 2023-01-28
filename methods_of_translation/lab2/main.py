from Analyzer.GraphViz import GraphViz

# if __name__ == '__main__':
#     s = input("Enter expression:\n")

if __name__ == '__main__':
    s = "(a in b) or (c not in b)"
    GraphViz().draw_on_string(s)
