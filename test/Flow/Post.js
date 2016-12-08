/* @flow */

import Comment from 'Comment';

export default type Post =
  { id: number
  , name: string
  , age: ?number
  , comments: Array<Comment>
  , promoted: ?Comment
  , author: ?string
  }
