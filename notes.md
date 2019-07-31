**URL**: [/users/gillchristian/gists](https://api.github.com/users/gillchristian/gists)

**Headers** (pagination):
```
-- when there's next
Link: <https://api.github.com/user/8309423/gists?page=2>; rel="next", <https://api.github.com/user/8309423/gists?page=2>; rel="last"

-- when there isn't next
Link: <https://api.github.com/user/8309423/gists?page=1>; rel="prev", <https://api.github.com/user/8309423/gists?page=1>; rel="first"
```

**Body**:

```json
{
  "id": "b731348066aee283f864d6541ee5083c",
  "description": "",
  "html_url": "https://gist.github.com/b731348066aee283f864d6541ee5083c",
  "files": {
    "for-vs-reduce.diff": {
      "filename": "for-vs-reduce.diff",
      "type": "text/plain",
      "language": "Diff",
      "raw_url": "https://gist.githubusercontent.com/gillchristian/b731348066aee283f864d6541ee5083c/raw/62f2170913f6600b945b893a5e38173f4913a380/for-vs-reduce.diff",
      "size": 150
    }
  },
  "public": true,
  "created_at": "2019-02-22T22:05:30Z",
  "updated_at": "2019-03-06T10:56:12Z",
  "owner": {
    "id": 8309423,
    "login": "gillchristian",
    "html_url": "https://github.com/gillchristian",
    "avatar_url": "https://avatars2.githubusercontent.com/u/8309423?v=4",
    // -----
    "gravatar_id": "",
    "node_id": "MDQ6VXNlcjgzMDk0MjM=",
    "url": "https://api.github.com/users/gillchristian",
    "followers_url": "https://api.github.com/users/gillchristian/followers",
    "following_url": "https://api.github.com/users/gillchristian/following{/other_user}",
    "gists_url": "https://api.github.com/users/gillchristian/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/gillchristian/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/gillchristian/subscriptions",
    "organizations_url": "https://api.github.com/users/gillchristian/orgs",
    "repos_url": "https://api.github.com/users/gillchristian/repos",
    "events_url": "https://api.github.com/users/gillchristian/events{/privacy}",
    "received_events_url": "https://api.github.com/users/gillchristian/received_events",
    "type": "User",
    "site_admin": false
  },
  // -----
  "url": "https://api.github.com/gists/b731348066aee283f864d6541ee5083c",
  "forks_url": "https://api.github.com/gists/b731348066aee283f864d6541ee5083c/forks",
  "commits_url": "https://api.github.com/gists/b731348066aee283f864d6541ee5083c/commits",
  "node_id": "MDQ6R2lzdGI3MzEzNDgwNjZhZWUyODNmODY0ZDY1NDFlZTUwODNj",
  "git_pull_url": "https://gist.github.com/b731348066aee283f864d6541ee5083c.git",
  "git_push_url": "https://gist.github.com/b731348066aee283f864d6541ee5083c.git",
  "comments": 0,
  "user": null,
  "comments_url": "https://api.github.com/gists/b731348066aee283f864d6541ee5083c/comments",
  "truncated": false
}
```

---

**TODO**:

v1

- [x] Hide token after save (allow to modify / clear)
- [x] Save token separately from the search (what did I mean here?)
- [x] Show a label similar to GitHub's (solve problem with layout) -> for secret ones
- [x] Have a "form" field with the WIP values (solved with the `ConfigFied` type)
- [x] Better position of the hamburger menu
- [ ] Better label for "Show only main Gist file" / "Show all files in Gist"
- [ ] Sidebar Design
- [ ] Footer -> Name & link to GitHub, Twitter, etc.
- [ ] Mobile friendly :sweat_smile:
- [ ] Fix: sidebar cannot be closed on mobile
- [ ] Initial blank screen (no user / no token) doesn't explain what to do
- [ ] Fix: Add token without user :bug:
- [ ] Fix: Reload when only token is present :bug:
- [ ] UX: Add sticky navbar (hamburger menu shows on top of things)
- [ ] UX: Be clear how to search, token is only for secret gists
- [ ] UX: Token should be enough to search (since it's got your user)
- [ ] UI: Better animation (mobile looks bad)

v2

- [ ] Only allow extending view (e.g. showing extra files) on list view. Keep grid as clean as possible.
- [ ] Keep "current" user in URL ? -> Migrate to [`Browser.application`](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application)
- [ ] Keep searched users in "tabs" ?
- [ ] On list view show more data (creation date, description?, files in line/tree, comment count)
- [ ] Improve the _fetch next page_ logic & state (e.g. keep track of "loading next page" -> show loding at bottom & in count)
- [ ] Make _fetch all pages_ configurable
- [ ] Keep searched users in "tabs" ?
