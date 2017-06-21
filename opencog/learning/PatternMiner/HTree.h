/*
 * opencog/learning/PatternMiner/HTree.h
 *
 * Copyright (C) 2012 by OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Shujing Ke <rainkekekeke@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_PATTERNMINER_HTREE_H
#define _OPENCOG_PATTERNMINER_HTREE_H
#include <map>
#include <vector>

#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace std;

namespace opencog
{
     namespace PatternMining
    {

     class  HTreeNode;

     struct ExtendRelation // to store a super pattern of a pattern, only store when it's extended from a const
     {
         HTreeNode* extendedHTreeNode; // the super pattern HTreeNode
         Handle sharedLink; // the link in original pattern that connect to new extended Link
         Handle newExtendedLink; // in super pattern (contains variables, not the instance link), without unifying
         Handle extendedNode; // the node that being extended in the original AtomSpace (the value node, not its variable name node)
     //    bool isExtendedFromVar; // if it's
     };

     class HTreeNode
         {
         public:
            HandleSeq pattern;
            vector<HandleSeq> instances; // the corresponding instances of this pattern in the original AtomSpace, only be used by breadth first mining
            set<HTreeNode*> parentLinks;
            set<HTreeNode*> childLinks;
            // set<string> instancesUidStrings;// all uid in each instance HandleSeq in all instances, in the form of 5152_815_201584. to prevent the same instance being count multiple times

            vector<ExtendRelation> superPatternRelations; // store all the connections to its super patterns

            unsigned int count; // instance number
            unsigned int var_num; // the number of all the variables in this pattern
            double interactionInformation;
            float nI_Surprisingness;
            float nII_Surprisingness;

            HandleSeq sharedVarNodeList; // all the shared nodes in these links in the original AtomSpace, each handle is a shared node

            HTreeNode()
            {
                parentLinks.clear();
                childLinks.clear();
                instances.clear();
                // instancesUidStrings.clear();
                superPatternRelations.clear();
                count = 0;
                var_num = 0;
                interactionInformation = 0.0;
                nI_Surprisingness = 0.0f;
                nII_Surprisingness = 0.0f;

            }

         };
     class HTree
     {

     public:

         HTreeNode* rootNode;

         HTree()
         {
             rootNode = new HTreeNode(); // the rootNode with no parents
         }

     };


    }
}

#endif //_OPENCOG_PATTERNMINER_HTREE_H
