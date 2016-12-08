/* @flow */

export default type Comment =
  { postId: number
  , text: string
  , mainCategories: tuple [string, string]
  , published: boolean
  , created: Date
  , tags: {[string]: (number)}
  }
