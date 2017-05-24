namespace Aardvark.Reconstruction

open System
open Aardvark.Base

module Match =
    
    let featureSets (l : list<FeatureNode>) (r : list<FeatureNode>) : list<FeatureNode*FeatureNode> =
        failwith ""
    
    let mkTracks (cameras : MapExt<CameraId, list<FeatureNode>>) 
                 (getMatches : list<FeatureNode> -> list<FeatureNode> -> list<FeatureNode * FeatureNode>)
                 : MapExt<CameraId, MapExt<TrackId, FeatureNode>> =

        let cids = cameras |> MapExt.toSeq |> Seq.map fst
        
        let allPairs =
            [
                for lcid in cids do
                    for rcid in cids do
                        if rcid.Id > lcid.Id then
                            yield (lcid, rcid)
            ]
        
        //dont filter out anything here. haha.
        let allEdges =
            allPairs |> List.map ( fun (lcid, rcid) ->
                let matches = getMatches cameras.[lcid] cameras.[rcid]
                { i0 = lcid.Id; i1 = rcid.Id; weight = matches }
            )

        let (tree,bestEdges) = 
            Graph.ofEdges allEdges
                |> Graph.minimumSpanningTree ( fun lWeight rWeight -> compare lWeight.Length rWeight.Length )
        
        //fill in correspondences
        for e in bestEdges do
            for (lNode, rNode) in e.weight do
                lNode.Add(CameraId(e.i1), rNode)
                rNode.Add(CameraId(e.i0), lNode)






        failwith ""
        