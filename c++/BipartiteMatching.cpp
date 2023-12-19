struct BipartiteMatching
/*
軽量化Dinic法
ref : https://snuke.hatenablog.com/entry/2019/05/07/013609
*/
{
    vector<int> pre, root;
    vector<vector<int>> to;
    vector<int> p, q;
    int n, m;
    BipartiteMatching(int n, int m) : pre(n, -1), root(n, -1), to(n), p(n, -1), q(m, -1), n(n), m(m) {}
    void add(int a, int b) { to[a].push_back(b); }
    int solve()
    {
        int res = 0;
        bool upd = true;
        while (upd)
        {
            upd = false;
            queue<int> s;
            for (int i = 0; i < n; ++i)
            {
                if (!~p[i])
                {
                    root[i] = i;
                    s.push(i);
                }
            }
            while (!s.empty())
            {
                int v = s.front();
                s.pop();
                if (~p[root[v]])
                    continue;
                for (int i = 0; i < (int)to[v].size(); ++i)
                {
                    int u = to[v][i];
                    if (!~q[u])
                    {
                        while (~u)
                        {
                            q[u] = v;
                            swap(p[v], u);
                            v = pre[v];
                        }
                        upd = true;
                        ++res;
                        break;
                    }
                    u = q[u];
                    if (~pre[u])
                        continue;
                    pre[u] = v;
                    root[u] = root[v];
                    s.push(u);
                }
            }
            if (upd)
                fill(pre.begin(), pre.end(), -1), fill(root.begin(), root.end(), -1);
        }
        return res;
    }
};